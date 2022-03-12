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
    use BasicJson/Internal/ContState.ContState;
    use BasicJson/Internal/ContState.ContState.{ContState};

    pub type alias JsonWriter[ka, a] = ContState[ka, Chain[JsonValue], a]
    pub type alias JsonObjectWriter[ka, a] = ContState[ka, Map[String, JsonValue], a]


}


namespace BasicJson/JsonWriter {

    use BasicJson.JsonWriter;
    use BasicJson.JsonObjectWriter;
    
    use BasicJson/Internal/ContState.ContState;
    use BasicJson.JsonValue;
    use BasicJson.JsonValue.{JsonObject, JsonString, JsonBool, JsonArray};
    use BasicJson.MarshalJson;

    pub def flatMap(f: a -> ContState[ka, s, b] & ef, ma: ContState[ka, s, a]): ContState[ka, s, b] & ef =
        Monad.flatMap(f, ma)


    pub def runJsonWriter(ma: JsonWriter[(Chain[BasicJson.JsonValue], a), a]): (JsonValue, a) & Impure =
        use Chain.ViewLeft.{NoneLeft, SomeLeft};
        let (w, ans) = BasicJson/Internal/ContState.runContState(ma, Chain.empty());
        match Chain.viewLeft(w) {
            case SomeLeft(x, rs) if Chain.isEmpty(rs) => (x, ans)
            case _                                    => (JsonArray(Chain.toList(w)), ans)
        }


    def runJsonObjectWriter(ma: JsonObjectWriter[(Map[String, BasicJson.JsonValue], a), a]): (JsonValue, a) & Impure =
        let (w, ans) = BasicJson/Internal/ContState.runContState(ma, Map.empty());
        (JsonObject(w), ans)

    pub def putFieldWith(name: String, ma: JsonWriter[(Chain[BasicJson.JsonValue], a), a]): JsonObjectWriter[ka, a] & Impure =
        use Applicative.{*>, point};
        let (w1, ans) = runJsonWriter(ma);
        BasicJson/Internal/ContState.update(s -> Map.insert(name, w1, s)) *> point(ans)
        

    def sequenceAx(xs: t[f[a]]): f[Unit] with Foldable[t], Applicative[f] =
        Foldable.foldLeft((ac, mx) -> Applicative.productRight(mx, ac), Applicative.point(), xs)


    pub def putObjectWith(ma: JsonObjectWriter[(Map[String, BasicJson.JsonValue], a), a]): JsonWriter[ka, a] & Impure = 
        use Applicative.{*>, point};
        let (w1, ans) = runJsonObjectWriter(ma);
        BasicJson/Internal/ContState.update(s -> Chain.snoc(s, w1)) *> point(ans)

    pub def putArrayWith(xs: List[JsonWriter[(Chain[BasicJson.JsonValue], Unit), Unit]]): JsonWriter[ka, Unit] & Impure = 
        use Applicative.{*>, point};
        let (w1, _) = runJsonWriter(sequenceAx(xs));
        /// runJsonWriter might have reduced w1 to a singleton rather than an array...
        let w2 = match w1 {
            case JsonArray(_) => w1
            case _            => JsonArray(w1 :: Nil)
        };
        BasicJson/Internal/ContState.update(s -> Chain.snoc(s, w2))

    pub def putArrayValues(xs: List[a]): JsonWriter[ka, Unit] with MarshalJson[a] =
        let arr = JsonArray(List.map(BasicJson/MarshalJson.toJsonValue, xs));
        BasicJson/Internal/ContState.update(w -> Chain.snoc(w, arr))

        
    pub def putBool(b: Bool): JsonWriter[ka, Unit] = 
        BasicJson/Internal/ContState.update(s -> Chain.snoc(s, JsonBool(b)))


    pub def putString(s: String): JsonWriter[ka, Unit] =
        BasicJson/Internal/ContState.update(w -> Chain.snoc(w, JsonString(s)))

    pub def putValue(x: a): JsonWriter[ka, Unit] with MarshalJson[a] =
        BasicJson/Internal/ContState.update(w -> Chain.snoc(w, BasicJson/MarshalJson.toJsonValue(x)))


}
