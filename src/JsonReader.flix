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
    
    /// Pure - JsonArry is internally a list so no need for impure array access...
    pub opaque type JsonReader[a] = BasicJson.JsonValue -> Result[a, String]

}    

namespace BasicJson/JsonReader {

    use BasicJson.JsonReader;
    use BasicJson.JsonReader.{JsonReader};

    use BasicJson.JsonValue;
    use BasicJson.JsonValue.{JsonObject, JsonArray, JsonString, 
        JsonFloat, JsonInt, JsonBool, JsonNull};

    def apply1(ma: JsonReader[a], jsv: JsonValue): Result[a, String] = 
        let JsonReader(x1) = ma;
        x1(jsv)

    pub def runJsonReader(ma: JsonReader[a], jsv: JsonValue): Result[a, String] = apply1(ma, jsv)
    
    pub def readJsonFile(ma: JsonReader[a], path: System.FilePath): Result[a, String] & Impure = 
        use Result.{flatMap};
        let* jsv = BasicJson/Parser.parseJsonFile(4, path);
        runJsonReader(ma, jsv)

    pub def return(x: a): JsonReader[a] = 
        JsonReader(_ -> Ok(x))

    pub def throwError(msg: String): JsonReader[a] =
        JsonReader(_ -> Err(msg))

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
                case Ok(a) => { let ans = fn(a) as & Pure; Ok(ans) }
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def ap(mf: JsonReader[a -> b & ef], ma: JsonReader[a]): JsonReader[b] & ef = 
        JsonReader(jsv -> 
            match apply1(mf, jsv) { 
                case Ok(f) => match apply1(ma, jsv) { 
                    case Ok(a) => { let ans = f(a) as & Pure; Ok(ans) }
                    case Err(e) => Err(e)
                }
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def seqLeft(ma: JsonReader[a], mb: JsonReader[b]): JsonReader[a] = 
        JsonReader(jsv -> 
            match apply1(ma, jsv) { 
                case Ok(a) => match apply1(mb, jsv) {
                    case Ok(_) => Ok(a)
                    case Err(e) => Err(e)
                }
                case Err(e) => Err(e)
            }
        )

    pub def seqRight(ma: JsonReader[a], mb: JsonReader[b]): JsonReader[b] = 
        JsonReader(jsv -> 
            match apply1(ma, jsv) { 
                case Ok(_) => apply1(mb, jsv)
                case Err(e) => Err(e)
            }
        )


    pub def bind(ma: JsonReader[a], fn: a -> JsonReader[b]): JsonReader[b] = 
        JsonReader(jsv -> 
            match apply1(ma, jsv) { 
                case Ok(a) => apply1(fn(a), jsv)
                case Err(e) => Err(e)
            }
        )

    pub def flatMap(fn: a -> JsonReader[b], ma: JsonReader[a]): JsonReader[b] = bind(ma, fn)

    pub def kleisliLeft(f1: a -> JsonReader[b], f2: b -> JsonReader[c], x: a): JsonReader[c] =
        JsonReader(jsv -> 
            match apply1(f1(x), jsv) { 
                case Ok(b) => apply1(f2(b), jsv)
                case Err(e) => Err(e)
            }
        )

    pub def kleisliRight(f1: b -> JsonReader[c], f2: a -> JsonReader[b], x: a): JsonReader[c] =
        kleisliLeft(f2, f1, x)

    pub def liftOption(x: Option[a], errMsg: String): JsonReader[a] =  match x {
        case Some(a) => return(a)
        case None    => fail(errMsg)
    }

    pub def liftResult(x: Result[a, e], makeErrMsg: e -> String): JsonReader[a] =  match x {
        case Ok(a) => return(a)
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
        alt(map(Some, ma), return(None))

    pub def getWithDefault(ma: JsonReader[a], d: a): JsonReader[a] = 
        alt(ma, return(d))

    /// Alias for `seqRight`
    pub def *>(p1: JsonReader[a], p2: JsonReader[b]): JsonReader[b] = 
        seqRight(p1, p2)

    /// Alias for `seqLeft`
    pub def <*(p1: JsonReader[a], p2: JsonReader[b]): JsonReader[a] = 
        seqLeft(p1, p2)

    pub def <!>(p1: JsonReader[a], p2: JsonReader[a]): JsonReader[a] =
        alt(p1, p2)

    // Haskell's <$>
    pub def <&>(f: a -> b, p2: JsonReader[a]): JsonReader[b] =
        map(f, p2)

    pub def <&(x: b, p1: JsonReader[a]): JsonReader[b] =
        p1 *> return(x)

    pub def &>(p1: JsonReader[a], x: b): JsonReader[b] =
        p1 *> return(x)

    /// Flipped <&>
    pub def <&&>(p2: JsonReader[a], f: a -> b): JsonReader[b] =
        map(f, p2)


    // Haskell's ap <*>
    pub def <*>(mf: JsonReader[a -> b], p2: JsonReader[a]): JsonReader[b] =
        ap(mf, p2)

    /// Flipped <*>
    pub def <**>(p2: JsonReader[a], mf: JsonReader[a -> b]): JsonReader[b] =
        ap(mf, p2)        


    pub def >>=(p: JsonReader[a], k: a -> JsonReader[b]): JsonReader[b] =
        bind(p, k)


    pub def =<<(k: a -> JsonReader[b], p: JsonReader[a]): JsonReader[b] =
        bind(p, k)

    pub def >=>(f1: a -> JsonReader[b], f2: b -> JsonReader[c], x: a): JsonReader[c] =
        kleisliLeft(f1, f2, x)

    pub def <=<(f1: b -> JsonReader[c], f2: a -> JsonReader[b], x: a): JsonReader[c] = 
        kleisliRight(f1, f2, x)

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
            case JsonNull => Ok()
            case _ => Err("getNull")
        })

    pub def getBool(): JsonReader[Bool] = 
        JsonReader(jsv  -> match jsv { 
            case JsonBool(b) => Ok(b)
            case _ => Err("getBool")
        })

    pub def getString(): JsonReader[String] = 
        JsonReader(jsv  -> match jsv { 
            case JsonString(s) => Ok(s)
            case _ => Err("getString")
        })

    /// Reads both ints and floats
    pub def getNumber(): JsonReader[Float64] = 
        JsonReader(jsv  -> match jsv { 
            case JsonFloat(d) => Ok(d)
            case JsonInt(i) => Ok(Int64.toFloat64(i))
            case _ => Err("getNumber")
        })

    pub def getFloat(): JsonReader[Float64] = 
        JsonReader(jsv  -> match jsv { 
            case JsonFloat(d) => Ok(d)
            case _ => Err("getNumber")
        })

    pub def getInteger(): JsonReader[Int64] = 
        JsonReader(jsv  -> match jsv {
            case JsonInt(i) => Ok(i)
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
                        sk: List[a] -> Result[List[a], String]): Result[List[a], String] = 
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
        JsonReader(jsv  -> if (test(jsv)) Ok(jsv) else Err("satisfy"))

    /// TODO - which Parsec style combinators are useful, especially if we anticipate
    /// "semi-structured" reading / traversal?



}
