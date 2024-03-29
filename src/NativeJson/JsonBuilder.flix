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


namespace NativeJson/JsonBuilder {

    use NativeJson.JsonValue;
    use NativeJson.JsonValue.{JsonObject, JsonArray};
    use NativeJson.MarshalJson;

    /// This module is underpowered - maybe we should use a monad so we can have conditional "put", etc.

    /// Note - may be wrapped as opaque at some point...
    pub type alias JsObject = Map[String, JsonValue]

    pub def createObject(): JsObject = Map.empty()

    pub def putField(name: String, value: a, obj: JsObject): JsObject with MarshalJson[a] = 
        Map.insert(name, NativeJson/MarshalJson.toJsonValue(value), obj)


    pub def objectOf(x: JsObject): JsonValue = JsonObject(x)

    pub def arrayOf(xs: List[a]): JsonValue with MarshalJson[a] = 
        JsonArray(List.map(NativeJson/MarshalJson.toJsonValue, xs))


    pub def arrayOfObject(xs: List[JsObject]): JsonValue = 
        JsonArray(List.map(objectOf, xs))


}
