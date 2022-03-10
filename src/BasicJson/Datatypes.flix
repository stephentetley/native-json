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
            BasicJson/Internal/StringWriter.exec(BasicJson/Datatypes.writeToString(x))
    }
}

namespace BasicJson/Datatypes {
    
    use BasicJson/Internal.StringWriter;
    use BasicJson.JsonValue;
    use BasicJson.JsonValue.{JsonObject, JsonArray, JsonString, JsonFloat, 
        JsonInt, JsonBool, JsonNull};


    /// TODO is this efficient as it uses `toString` between recursive levels?
    /// TODO should be pure?
    pub def writeToString(x: JsonValue): StringWriter[Unit] = 
        use BasicJson/Internal/StringWriter.{writeString, writeParens, writes, writesList, writesMap};
        use BasicJson/Internal/StringWriter.{*>};
        match x { 
            case JsonObject(kvs) => writeString("JsonObject") *> writeParens(true, writesMap(kvs))
            case JsonArray(xs)   => writeString("JsonArray")  *> writeParens(true, writesList(xs))
            case JsonString(s)   => writeString("JsonString") *> writeParens(true, writeString(s))
            case JsonFloat(d)    => writeString("JsonFloat")  *> writeParens(true, writes(d))
            case JsonInt(n)      => writeString("JsonInt")    *> writeParens(true, writes(n))
            case JsonBool(b)     => writeString("JsonBool")   *> writeParens(true, if (b) writeString("true") else writeString("false"))
            case JsonNull        => writeString("JsonNull")
            case _               => bug!("BasicJson/Datatypes.writeToString - match fail")
        }

}
