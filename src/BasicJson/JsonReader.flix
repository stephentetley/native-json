/*
 * Copyright 2021 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace BasicJson {
    
    /// This is Impure so we aren't casting to Pure.
    /// Ideally we would track `ef` in the type.
    pub opaque type JsonReader[a] = BasicJson.JsonValue -> Result[a, String] & Impure

    instance Functor[JsonReader] {
        pub def map(f: a -> b & ef, x: JsonReader[a]): JsonReader[b] & ef = 
          BasicJson/JsonReader.map(f, x)  
    }

    instance Applicative[JsonReader] {
        pub def point(x: a): JsonReader[a] = BasicJson/JsonReader.point(x)

        pub def ap(mf: JsonReader[a -> b & ef], ma: JsonReader[a]): JsonReader[b] & ef =
            BasicJson/JsonReader.ap(mf, ma)
    }

    instance Monad[JsonReader] {
        pub def flatMap(f: a -> JsonReader[b] & ef, ma: JsonReader[a]): JsonReader[b] & ef =
            BasicJson/JsonReader.flatMap(f, ma)
    }

}    

namespace BasicJson/JsonReader {

    use BasicJson.JsonReader;
    use BasicJson.JsonReader.{JsonReader};

    use BasicJson.JsonValue;
    use BasicJson.JsonValue.{JsonObject, JsonArray, JsonString, 
        JsonFloat, JsonInt, JsonBool, JsonNull};

    def apply1(ma: JsonReader[a], jsv: JsonValue): Result[a, String] & Impure = 
        let JsonReader(x1) = ma;
        x1(jsv)

    pub def runJsonReader(ma: JsonReader[a], jsv: JsonValue): Result[a, String] & Impure = apply1(ma, jsv)
    
    pub def readJsonFile(ma: JsonReader[a], path: System.FilePath): Result[a, String] & Impure = 
        use Result.{flatMap};
        let* jsv = BasicJson/Parser.parseJsonFile(4, path);
        runJsonReader(ma, jsv)

    pub def point(x: a): JsonReader[a] = 
        JsonReader(_ -> Ok(x) as & Impure)

    pub def throwError(msg: String): JsonReader[a] =
        JsonReader(_ -> Err(msg) as & Impure)

    pub def fail(message: String) : JsonReader[a] = throwError(message)

    pub def catchError(ma: JsonReader[a], handle: String -> JsonReader[a]): JsonReader[a] =
        JsonReader(jsv -> match apply1(ma, jsv) { 
            case Ok(a) => Ok(a)
            case Err(e) => apply1(handle(e), jsv)
        })

    pub def mapError(f: String -> String, ma: JsonReader[a]): JsonReader[a] =
        JsonReader(jsv -> match apply1(ma, jsv) { 
            case Ok(a) => Ok(a)
            case Err(e) => Err(f(e))
        })

    /// Needs to allow allow effects (e.g create / access arrays)
    pub def map(fn: a -> b & ef, ma: JsonReader[a]): JsonReader[b] & ef = 
        JsonReader(jsv -> 
            match apply1(ma, jsv) { 
                case Ok(a) => { let ans = fn(a) as & Impure; Ok(ans) }
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def ap(mf: JsonReader[a -> b & ef], ma: JsonReader[a]): JsonReader[b] & ef = 
        JsonReader(jsv -> 
            match apply1(mf, jsv) { 
                case Ok(f) => match apply1(ma, jsv) { 
                    case Ok(a) => { let ans = f(a) as & Impure; Ok(ans) }
                    case Err(e) => Err(e)
                }
                case Err(e) => Err(e)
            }
        ) as & ef



    pub def bind(ma: JsonReader[a], fn: a -> JsonReader[b] & ef): JsonReader[b] & ef= 
        JsonReader(jsv -> 
            match apply1(ma, jsv) { 
                case Ok(a) => apply1(fn(a) as & Impure, jsv) 
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def flatMap(fn: a -> JsonReader[b] & ef, ma: JsonReader[a]): JsonReader[b] & ef = bind(ma, fn)


    pub def liftOption(x: Option[a], errMsg: String): JsonReader[a] =  match x {
        case Some(a) => point(a)
        case None    => fail(errMsg)
    }

    pub def liftResult(x: Result[a, e], makeErrMsg: e -> String): JsonReader[a] =  match x {
        case Ok(a) => point(a)
        case Err(e) => fail(makeErrMsg(e))
    }


    pub def alt(ma: JsonReader[a], mb: JsonReader[a]): JsonReader[a] =
        JsonReader(jsv -> 
            match apply1(ma, jsv) { 
                case Ok(a) => Ok(a)
                case Err(_) => apply1(mb, jsv)
            }
        )

    pub def optional(ma: JsonReader[a]): JsonReader[Option[a]] = 
        alt(map(Some, ma), point(None))

    pub def getWithDefault(ma: JsonReader[a], d: a): JsonReader[a] = 
        alt(ma, point(d))


    pub def <!>(p1: JsonReader[a], p2: JsonReader[a]): JsonReader[a] =
        alt(p1, p2)


    /// ## Elemetary Readers

    pub def withField(name: String, ma: JsonReader[a]): JsonReader[a] = 
        JsonReader(jsv  -> match jsv { 
            case JsonObject(kvs) => match Map.get(name, kvs) {
                case Some(jsv) => apply1(ma, jsv)
                case None => Err("withField - missing key - '${name}'")
            }
            case _ => Err("withField - not an object")
        })


    pub def getNull(): JsonReader[Unit] = 
        JsonReader(jsv  -> match jsv { 
            case JsonNull => Ok() as & Impure
            case _ => Err("getNull")
        })

    pub def getBool(): JsonReader[Bool] = 
        JsonReader(jsv  -> match jsv { 
            case JsonBool(b) => Ok(b) as & Impure
            case _ => Err("getBool")
        })

    pub def getString(): JsonReader[String] = 
        JsonReader(jsv  -> match jsv { 
            case JsonString(s) => Ok(s) as & Impure
            case _ => Err("getString")
        })

    /// Reads both ints and floats
    pub def getNumber(): JsonReader[Float64] = 
        JsonReader(jsv  -> match jsv { 
            case JsonFloat(d) => Ok(d) as & Impure
            case JsonInt(i) => Ok(Int64.toFloat64(i))
            case _ => Err("getNumber")
        })

    pub def getFloat(): JsonReader[Float64] = 
        JsonReader(jsv  -> match jsv { 
            case JsonFloat(d) => Ok(d) as & Impure
            case _ => Err("getNumber")
        })

    pub def getInteger(): JsonReader[Int64] = 
        JsonReader(jsv  -> match jsv {
            case JsonInt(i) => Ok(i) as & Impure
            case _ => Err("getInteger")
        })

    


    pub def getInt8(): JsonReader[Int8] =
        let* n = getInteger();
        liftOption(Int64.tryToInt8(n), "getInt8 - out of Int32 range")

    pub def getInt16(): JsonReader[Int16] =
        let* n = getInteger();
        liftOption(Int64.tryToInt16(n), "getInt16 - out of Int32 range")

    pub def getInt32(): JsonReader[Int32] = 
        let* n = getInteger();
        liftOption(Int64.tryToInt32(n), "getInt32 - out of Int32 range")

    pub def getInt64(): JsonReader[Int64] = getInteger()

    pub def getFloat32(): JsonReader[Float32] = 
        let* n = getNumber();
        liftOption(Float64.tryToFloat32(n), "getFloat32 - out of Float32 range")

    pub def getFloat64(): JsonReader[Float64] = getNumber()
    
    pub def withArray(ma: JsonReader[a]): JsonReader[List[a]] = 
        JsonReader(jsv  -> match jsv {
            case JsonArray(xs) => withArrayHelper(ma, xs, msg -> Err(msg), ks -> Ok(ks))
            case _ => Err("withArray")
        })

    def withArrayHelper(ma: JsonReader[a], 
                        xs: List[JsonValue], 
                        fk: String-> Result[List[a], String], 
                        sk: List[a] -> Result[List[a], String]): Result[List[a], String] & Impure = 
        match xs { 
            case Nil => sk(Nil)
            case x :: rs => match apply1(ma, x) { 
                case Ok(x1) => withArrayHelper(ma, rs, fk, ks -> sk(x1 :: ks))
                case Err(msg) => fk(msg)
            }

        }

    pub def filePath(): JsonReader[System.FilePath] = 
        map(System/FilePath.new, getString())

    /// Date is a JsonString
    pub def localDate(formatter: Time/Format.DateFormatter): JsonReader[Time.LocalDate] = 
        let* s = getString();
        liftOption(Time/LocalDate.parse(formatter, s), "localDate - parse error")

    pub def localTime(formatter: Time/Format.TimeFormatter): JsonReader[Time.LocalTime] = 
        let* s = getString();
        liftOption(Time/LocalTime.parse(formatter, s), "localTime - parse error")

    pub def localDateTime(formatter: Time/Format.DateTimeFormatter): JsonReader[Time.LocalDateTime] = 
        let* s = getString();
        liftOption(Time/LocalDateTime.parse(formatter, s), "localDateTime - parse error")

    pub def satisfy(test: JsonValue -> Bool): JsonReader[JsonValue] = 
        JsonReader(jsv  -> if (test(jsv)) (Ok(jsv) as & Impure) else Err("satisfy"))

    /// TODO - which Parsec style combinators are useful, especially if we anticipate
    /// "semi-structured" reading / traversal?



}
