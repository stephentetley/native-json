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

    use BasicJson.JsonValue;
    use BasicJson.JsonValue.{JsonObject, JsonArray, JsonString, 
        JsonFloat, JsonInt, JsonBool, JsonNull};

    pub lawless class MarshalJson[a] {
        pub def toJsonValue(x: a): JsonValue
        pub def fromJsonValue(x: JsonValue): Result[a, String]
    }

    instance MarshalJson[JsonValue] {
        pub def toJsonValue(x: JsonValue): JsonValue = x
        pub def fromJsonValue(x: JsonValue): Result[JsonValue, String] = Ok(x)
    }


    instance MarshalJson[Bool] {
        pub def toJsonValue(x: Bool): JsonValue = JsonBool(x)

        pub def fromJsonValue(x: JsonValue): Result[Bool, String] = match x {
            case JsonBool(b) => Ok(b)
            case _ => Err("Unable to read Bool")
        }
    }

    instance MarshalJson[String] {
        pub def toJsonValue(x: String): JsonValue = JsonString(x)

        pub def fromJsonValue(x: JsonValue): Result[String, String] = match x {
            case JsonString(s) => Ok(s)
            case _ => Err("Unable to read String")
        }
    }

    instance MarshalJson[Int32] {
        pub def toJsonValue(x: Int32): JsonValue = JsonInt(Int32.toInt64(x))

        pub def fromJsonValue(x: JsonValue): Result[Int32, String] = match x {
            case JsonInt(d) => Option.toOk(Int64.tryToInt32(d), "Unable to read Int32")
            case _ => Err("Unable to read Int32")
        }
    }

    instance MarshalJson[Float64] {
        pub def toJsonValue(x: Float64): JsonValue = JsonFloat(x)

        pub def fromJsonValue(x: JsonValue): Result[Float64, String] = match x {
            case JsonFloat(d) => Ok(d)
            case _ => Err("Unable to read Int32")
        }
    }

    instance MarshalJson[List[a]] with MarshalJson[a] {
        pub def toJsonValue(xs: List[a]): JsonValue = 
            JsonArray(List.map(BasicJson/MarshalJson.toJsonValue, xs))

        pub def fromJsonValue(x: JsonValue): Result[List[a], String] = match x {
            case JsonArray(xs) => match Result.traverse(BasicJson/MarshalJson.fromJsonValue, xs) { 
                case Ok(ys) => Ok(ys)
                case Err(_) => Err("Unable to read List")
            }
            case _ => Err("Unable to read List")
        }
    }

}
