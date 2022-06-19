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
    /// JsonReader has a simple but useful notion of scope - if you are reading
    /// a JSON object the only primitive reader you can use is`getFieldWith`.
    ///
    pub enum JsonReader[_scope, a](BasicJson.JsonValue -> Result[a, String] & Impure)

    /// Note - probably we will need to move to a ContReader monad to handle
    /// large input.



    instance Functor[JsonReader[scope]] {
        pub def map(f: a -> b & ef, x: JsonReader[scope, a]): JsonReader[scope, b] & ef = 
          BasicJson/JsonReader.map(f, x)  
    }

    instance Applicative[JsonReader[scope]] {
        pub def point(x: a): JsonReader[scope, a] = BasicJson/JsonReader.point(x)

        pub def ap(mf: JsonReader[scope, a -> b & ef], ma: JsonReader[scope, a]): JsonReader[scope, b] & ef =
            BasicJson/JsonReader.ap(mf, ma)
    }

    instance Monad[JsonReader[scope]] {
        pub def flatMap(f: a -> JsonReader[scope, b] & ef, ma: JsonReader[scope, a]): JsonReader[scope, b] & ef =
            BasicJson/JsonReader.flatMap(f, ma)
    }

}    

namespace BasicJson/JsonReader {

    use Time/Format.DateTimeFormatter;

    use BasicJson.JsObject;
    use BasicJson.JsValue;


    use BasicJson.JsonReader;
    use BasicJson.JsonReader.{JsonReader};

    use BasicJson.JsonValue;
    use BasicJson.JsonValue.{JsonObject, JsonArray, JsonString, 
        JsonFloat, JsonInt, JsonBool, JsonNull};


    def apply1(ma: JsonReader[scope, a], jsv: JsonValue): Result[a, String] & Impure = 
        let JsonReader(x1) = ma;
        x1(jsv)

    pub def runJsonReader(ma: JsonReader[scope, a], jsv: JsonValue): Result[a, String] & Impure = apply1(ma, jsv)
    
    pub def readJsonFile(ma: JsonReader[scope, a], path: FlixSandbox/System.FilePath): Result[a, String] & Impure = 
        use Result.{flatMap};
        let* jsv = BasicJson/Parser.parseJsonFile(4, path);
        runJsonReader(ma, jsv)

    pub def point(x: a): JsonReader[scope, a] = 
        JsonReader(_ -> Ok(x) as & Impure)

    pub def throwError(msg: String): JsonReader[scope, a] =
        JsonReader(_ -> Err(msg) as & Impure)

    pub def fail(message: String) : JsonReader[scope, a] = throwError(message)

    pub def catchError(ma: JsonReader[scope, a], handle: String -> JsonReader[scope, a]): JsonReader[scope, a] =
        JsonReader(jsv -> match apply1(ma, jsv) { 
            case Ok(a) => Ok(a)
            case Err(e) => apply1(handle(e), jsv)
        })

    pub def mapError(f: String -> String, ma: JsonReader[scope, a]): JsonReader[scope, a] =
        JsonReader(jsv -> match apply1(ma, jsv) { 
            case Ok(a) => Ok(a)
            case Err(e) => Err(f(e))
        })

    /// Needs to allow allow effects (e.g create / access arrays)
    pub def map(fn: a -> b & ef, ma: JsonReader[scope, a]): JsonReader[scope, b] & ef = 
        JsonReader(jsv -> 
            match apply1(ma, jsv) { 
                case Ok(a) => { let ans = fn(a) as & Impure; Ok(ans) }
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def ap(mf: JsonReader[scope, a -> b & ef], ma: JsonReader[scope, a]): JsonReader[scope, b] & ef = 
        JsonReader(jsv -> 
            match apply1(mf, jsv) { 
                case Ok(f) => match apply1(ma, jsv) { 
                    case Ok(a) => { let ans = f(a) as & Impure; Ok(ans) }
                    case Err(e) => Err(e)
                }
                case Err(e) => Err(e)
            }
        ) as & ef



    pub def bind(ma: JsonReader[scope, a], fn: a -> JsonReader[scope, b] & ef): JsonReader[scope, b] & ef= 
        JsonReader(jsv -> 
            match apply1(ma, jsv) { 
                case Ok(a) => apply1(fn(a) as & Impure, jsv) 
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def flatMap(fn: a -> JsonReader[scope, b] & ef, ma: JsonReader[scope, a]): JsonReader[scope, b] & ef = bind(ma, fn)


    pub def liftOption(x: Option[a], errMsg: String): JsonReader[scope, a] =  match x {
        case Some(a) => point(a)
        case None    => fail(errMsg)
    }

    pub def liftResult(x: Result[a, String]): JsonReader[scope, a] =  match x {
        case Ok(a) => point(a)
        case Err(msg) => fail(msg)
    }

    pub def liftResult1(x: Result[a, e], makeErrMsg: e -> String): JsonReader[scope, a] =  match x {
        case Ok(a) => point(a)
        case Err(e) => fail(makeErrMsg(e))
    }

    pub def alt(ma: JsonReader[scope, a], mb: JsonReader[scope, a]): JsonReader[scope, a] =
        JsonReader(jsv -> 
            match apply1(ma, jsv) { 
                case Ok(a) => Ok(a)
                case Err(_) => apply1(mb, jsv)
            }
        )

    pub def optional(ma: JsonReader[scope, a]): JsonReader[scope, Option[a]] = 
        alt(map(Some, ma), point(None))

    pub def getWithDefault(ma: JsonReader[scope, a], d: a): JsonReader[scope, a] = 
        alt(ma, point(d))


    pub def <!>(p1: JsonReader[scope, a], p2: JsonReader[scope, a]): JsonReader[scope, a] =
        alt(p1, p2)


    /// ## Elemetary Readers

    pub def getObjectWith(ma: JsonReader[JsObject, a]): JsonReader[JsValue, a] = 
        JsonReader(jsv  -> match jsv { 
            case JsonObject(_) => apply1(ma, jsv)
            case _ => Err("getObjectWith - not an object")
        })

    pub def getFieldWith(name: String, ma: JsonReader[JsValue, a]): JsonReader[JsObject, a] = 
        JsonReader(jsv  -> match jsv { 
            case JsonObject(kvs) => match Map.get(name, kvs) {
                case Some(jsv1) => apply1(ma, jsv1)
                case None => Err("withField - missing key - '${name}'")
            }
            case _ => Err("withField - not an object")
        })


    pub def getNull(): JsonReader[JsValue, Unit] = 
        JsonReader(jsv  -> match jsv { 
            case JsonNull => Ok() as & Impure
            case _ => Err("getNull")
        })

    pub def getBool(): JsonReader[JsValue, Bool] = 
        JsonReader(jsv  -> match jsv { 
            case JsonBool(b) => Ok(b) as & Impure
            case _ => Err("getBool")
        })

    pub def getString(): JsonReader[JsValue, String] = 
        JsonReader(jsv  -> match jsv { 
            case JsonString(s) => Ok(s) as & Impure
            case _ => Err("getString")
        })

    /// Reads both ints and floats
    pub def getNumber(): JsonReader[JsValue, Float64] = 
        JsonReader(jsv  -> match jsv { 
            case JsonFloat(d) => Ok(d) as & Impure
            case JsonInt(i) => Ok(Int64.toFloat64(i))
            case _ => Err("getNumber")
        })

    pub def getFloat(): JsonReader[JsValue, Float64] = 
        JsonReader(jsv  -> match jsv { 
            case JsonFloat(d) => Ok(d) as & Impure
            case _ => Err("getNumber")
        })

    pub def getInteger(): JsonReader[JsValue, Int64] = 
        JsonReader(jsv  -> match jsv {
            case JsonInt(i) => Ok(i) as & Impure
            case _ => Err("getInteger")
        })

    pub def getInt8(): JsonReader[JsValue, Int8] =
        let* n = getInteger();
        liftOption(Int64.tryToInt8(n), "getInt8 - out of Int32 range")

    pub def getInt16(): JsonReader[JsValue, Int16] =
        let* n = getInteger();
        liftOption(Int64.tryToInt16(n), "getInt16 - out of Int32 range")

    pub def getInt32(): JsonReader[JsValue, Int32] = 
        let* n = getInteger();
        liftOption(Int64.tryToInt32(n), "getInt32 - out of Int32 range")

    pub def getInt64(): JsonReader[JsValue, Int64] = getInteger()

    pub def getFloat32(): JsonReader[JsValue, Float32] = 
        let* n = getNumber();
        liftOption(Float64.tryToFloat32(n), "getFloat32 - out of Float32 range")

    pub def getFloat64(): JsonReader[JsValue, Float64] = getNumber()
    
    pub def getArrayWith(ma: JsonReader[JsValue, a]): JsonReader[JsValue, List[a]] = 
        JsonReader(jsv  -> match jsv {
            case JsonArray(xs) => withArrayHelper(ma, xs, msg -> Err(msg), ks -> Ok(ks))
            case _ => Err("withArray")
        })

    def withArrayHelper(ma: JsonReader[scope, a], 
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

    pub def filePath(): JsonReader[JsValue, FlixSandbox/System.FilePath] = 
        map(FlixSandbox/System/FilePath.filePath, getString())

    /// Date is a JsonString
    pub def localDate(formatter: DateTimeFormatter[Time/Format.DateFormat]): JsonReader[JsValue, Time.LocalDate] = 
        let* s = getString();
        liftResult(Time/LocalDate.parse(formatter, s))

    pub def localTime(formatter: DateTimeFormatter[Time/Format.TimeFormat]): JsonReader[JsValue, Time.LocalTime] = 
        let* s = getString();
        liftResult(Time/LocalTime.parse(formatter, s))

    pub def localDateTime(formatter: DateTimeFormatter[Time/Format.DateTimeFormat]): JsonReader[JsValue, Time.LocalDateTime] = 
        let* s = getString();
        liftResult(Time/LocalDateTime.parse(formatter, s))


    pub def satisfy(test: JsonValue -> Bool): JsonReader[scope, JsonValue] = 
        JsonReader(jsv  -> if (test(jsv)) (Ok(jsv) as & Impure) else Err("satisfy"))

    /// TODO - which Parsec style combinators are useful, especially if we anticipate
    /// "semi-structured" reading / traversal?



}
