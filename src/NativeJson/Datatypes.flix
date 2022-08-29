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


namespace NativeJson {

    /// Scope types for JsonReader
    pub enum JsObject {}
    pub enum JsValue {}


    /// See - https://www.json.org/json-en.html
    /// Files must be UTF-8

    pub enum JsonValue with Eq, Order { 
        case JsonObject(Map[String, JsonValue])
        case JsonArray(List[JsonValue])
        case JsonString(String)
        case JsonFloat(Float64)
        case JsonInt(Int64) 
        case JsonBool(Bool)
        case JsonNull
    }

    instance ToString[JsonValue] {
        pub def toString(x: JsonValue): String = 
            region r {
                NativeJson/Internal/StringWriter.exec(NativeJson/Datatypes.writeToString(x), r)
            }
    }
}

namespace NativeJson/Datatypes {
    
    use NativeJson/Internal.StringWriter;
    use NativeJson.JsonValue;
    use NativeJson.JsonValue.{JsonObject, JsonArray, JsonString, JsonFloat, 
        JsonInt, JsonBool, JsonNull};


    /// TODO is this efficient as it uses `toString` between recursive levels?
    pub def writeToString(x: JsonValue): StringWriter[r, Unit] \ Write(r) = 
        use Applicative.{*>};
        use NativeJson/Internal/StringWriter.{writeString, writeParens, writes, writesList, writesMap};        
        match x { 
            case JsonObject(kvs) => writeString("JsonObject") *> writeParens(true, writesMap(kvs))
            case JsonArray(xs)   => writeString("JsonArray")  *> writeParens(true, writesList(xs))
            case JsonString(s)   => writeString("JsonString") *> writeParens(true, writeString(s))
            case JsonFloat(d)    => writeString("JsonFloat")  *> writeParens(true, writes(d))
            case JsonInt(n)      => writeString("JsonInt")    *> writeParens(true, writes(n))
            case JsonBool(b)     => writeString("JsonBool")   *> writeParens(true, if (b) writeString("true") else writeString("false"))
            case JsonNull        => writeString("JsonNull")
            case _               => bug!("NativeJson/Datatypes.writeToString - match fail")
        }

}
