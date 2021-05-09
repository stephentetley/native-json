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


namespace BasicJson/Syntax {

    /// See - https://www.json.org/json-en.html
    /// Files must be UTF-8

    pub enum JsonValue { 
        case JsonObject(Map[String, JsonValue])
        case JsonArray(List[JsonValue])
        case JsonString(String)
        case JsonNumber(Bool, Float64)
        case JsonFalse
        case JsonTrue
        case JsonNull
    }

}
