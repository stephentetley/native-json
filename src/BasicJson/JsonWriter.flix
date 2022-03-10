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
    
    use BasicJson.JsValue;

    pub opaque type JsonWriter[a] = Chain[JsonValue] -> (Chain[JsonValue], a) & Impure
    
    pub opaque type JsonObjectWriter[a] = Map[String, JsonValue] -> (Map[String, JsonValue], a) & Impure

    instance Functor[JsonObjectWriter] {
        pub def map(fn: a -> b & ef, ma: JsonObjectWriter[a]): JsonObjectWriter[b] & ef = 
            BasicJson/JsonObjectWriter.map(fn, ma)  
    }

}

namespace BasicJson/JsonObjectWriter {

    use BasicJson.JsonObjectWriter;
    use BasicJson.JsonObjectWriter.JsonObjectWriter;
    use BasicJson.JsonValue;

    def apply1(ma: JsonObjectWriter[a], s: Map[String, JsonValue]): (Map[String, JsonValue], a) & Impure = 
        let JsonObjectWriter(f) = ma;
        f(s)

    pub def point(x: a): JsonObjectWriter[a] = 
        JsonObjectWriter(w -> (w, x) as & Impure)


    /// Needs to allow allow effects (e.g create / access arrays)
    pub def map(fn: a -> b & ef, ma: JsonObjectWriter[a]): JsonObjectWriter[b] & ef = 
        JsonObjectWriter(w -> 
            let (w1, a) = apply1(ma, w);
            (w1, fn(a))
        ) as & ef

    pub def ap(mf: JsonObjectWriter[a -> b & ef], ma: JsonObjectWriter[a]): JsonObjectWriter[b] & ef = 
        JsonObjectWriter(w -> 
            let (w1, f) = apply1(mf, w);
            let (w2, a) = apply1(ma, w1);
            (w2, f(a) as & Impure)
        ) as & ef

    pub def bind(ma: JsonObjectWriter[a], fn: a -> JsonObjectWriter[b] & ef): JsonObjectWriter[b] & ef= 
        JsonObjectWriter(w -> 
            let (w1, a) = apply1(ma, w);
            apply1(fn(a) as & Impure, w1)
        ) as & ef

    pub def flatMap(fn: a -> JsonObjectWriter[b] & ef, ma: JsonObjectWriter[a]): JsonObjectWriter[b] & ef = bind(ma, fn)

}

namespace BasicJson/JsonWriter {

    use BasicJson.JsonWriter;
    use BasicJson.JsonWriter.JsonWriter;
    use BasicJson.JsonObjectWriter;
    use BasicJson.JsonObjectWriter.JsonObjectWriter;
    
    use BasicJson.JsonValue;
    use BasicJson.JsonValue.{JsonObject, JsonString, JsonBool, JsonArray};
    use BasicJson.MarshalJson;


    def apply1(ma: JsonWriter[a], s: Chain[JsonValue]): (Chain[JsonValue], a) & Impure = 
        let JsonWriter(f) = ma;
        f(s)

    pub def runJsonWriter(ma: JsonWriter[a]): (JsonValue, a) & Impure =
        use Chain.ViewLeft.{NoneLeft, SomeLeft};
        let (w, ans) = apply1(ma, Chain.empty());
        match Chain.viewLeft(w) {
            case SomeLeft(x, rs) if Chain.isEmpty(rs) => (x, ans)
            case NoneLeft                             => (JsonArray(Chain.toList(w)), ans)
        }
    
    def runJsonObjectWriter(ma: JsonObjectWriter[a]): (JsonValue, a) & Impure =
        let JsonObjectWriter(f) = ma;
        let (w, ans) = f(Map.empty());
        (JsonObject(w), ans)
    
    
    pub def putFieldWith(name: String, ma: JsonWriter[a]): JsonObjectWriter[a] =
        JsonObjectWriter(w ->
            let (w1, a) = runJsonWriter(ma);
            (Map.insert(name, w1, w), a)
        )

    /// pub def arrayOf(xs: List[a]): JsonValue with MarshalJson[a] = 
    ///     JsonArray(List.map(BasicJson/MarshalJson.toJsonValue, xs))


    /// pub def arrayOfObject(xs: List[JsObject]): JsonValue = 
    ///     JsonArray(List.map(objectOf, xs))

    pub def putObjectWith(ma: JsonObjectWriter[a]): JsonWriter[a] = 
        JsonWriter(w ->
            let (w1, a) = runJsonObjectWriter(ma);
            (Chain.snoc(w, w1), a)
        )

        
    pub def putBool(b: Bool): JsonWriter[Unit] = 
        JsonWriter(w -> (Chain.snoc(w, JsonBool(b)), ()) as & Impure)

    pub def putString(s: String): JsonWriter[Unit] =
        JsonWriter(w -> (Chain.snoc(w, JsonString(s)), ()) as & Impure)

    pub def putValue(x: a): JsonWriter[Unit] with MarshalJson[a] =
        JsonWriter(w -> (Chain.snoc(w, BasicJson/MarshalJson.toJsonValue(x)), ()) as & Impure)


}
