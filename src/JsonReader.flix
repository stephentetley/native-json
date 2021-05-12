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

    pub opaque type JsonReader[a] = BasicJson/Syntax.JsonValue ~> Result[a, String]

}    

namespace BasicJson/JsonReader {

    use BasicJson.JsonReader;
    use BasicJson.JsonReader.{JsonReader};

    use BasicJson/Syntax.JsonValue;
    use BasicJson/Syntax.JsonValue.{JsonObject, JsonArray, JsonString, 
        JsonNumber, JsonFalse, JsonTrue, JsonNull};

    def apply1(ma: JsonReader[a], jsv: JsonValue): Result[a, String] & Impure = 
        let JsonReader(x1) = ma;
        x1(jsv)

    pub def runReader(jsv: JsonValue, ma: JsonReader[a]): Result[a, String] & Impure = apply1(ma, jsv)

    pub def return(x: a): JsonReader[a] = 
        JsonReader(_ -> Ok(x) as & Impure)

    pub def throwError(msg: String): JsonReader[a] =
        JsonReader(_ -> Err(msg) as & Impure)


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
    pub def map(fn: a -> b & ef, ma: JsonReader[a]): JsonReader[b] = 
        JsonReader(jsv -> 
            match apply1(ma, jsv) { 
                case Ok(a) => { let ans = fn(a) as & Impure; Ok(ans) }
                case Err(e) => Err(e)
            }
        )

    pub def ap(mf: JsonReader[a -> b & ef], ma: JsonReader[a]): JsonReader[b] = 
        JsonReader(jsv -> 
            match apply1(mf, jsv) { 
                case Ok(f) => match apply1(ma, jsv) { 
                    case Ok(a) => { let ans = f(a) as & Impure; Ok(ans) }
                    case Err(e) => Err(e)
                }
                case Err(e) => Err(e)
            }
        )

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

    pub def getBool(): JsonReader[Bool] = 
        JsonReader(jsv  -> match jsv { 
            case JsonTrue => Ok(true)
            case JsonFalse => Ok(false)
            case _ => Err("getBool")
        } as & Impure)

    pub def getString(): JsonReader[String] = 
        JsonReader(jsv  -> match jsv { 
            case JsonString(s) => Ok(s)
            case _ => Err("getString")
        } as & Impure)

        
}
