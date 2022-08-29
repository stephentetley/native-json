

namespace JsonTest {

@test
def test01(): Int32 & Impure =
    use NativeJson/Pretty.{renderJson};
    NativeJson/MarshalJson.toJsonValue(1000.0f64) |> renderJson |> println;
    /// renderJson(true, 1000.0001f64) |> println;
    /// renderJson(String.unlines("Hello" :: "World" :: Nil)) |> println;
    0



def dquote(s: String): String = 
    Char.toString('\u0022') + s + Char.toString('\u0022')

@test
def test02(): Int32 & Impure =
    use NativeJson/Parser.{pValue};
    FlixParsec/Prim.parsesString(pValue(), "true") |> println;
    FlixParsec/Prim.parsesString(pValue(), "100.75") |> println;
    FlixParsec/Prim.parsesString(pValue(), "[true, null, 1000, false]") |> println;
    let key1 = dquote("field1");
    let key2 = dquote("field2");
    FlixParsec/Prim.parsesString(pValue(), "${key1}") |> println;
    FlixParsec/Prim.parsesString(pValue(), "{${key1}: 1000, ${key2}: false}") |> println;
    FlixParsec/Prim.parsesString(pValue(), "{${key1}: [true, null, 1000, false], ${key2}: false}") |> println;
    0

@test
def test03(): Int32 & Impure =
    use NativeJson/MarshalJson.toJsonValue;
    use NativeJson/JsonReader.{runJsonReader, getBool};
    let js1 = toJsonValue(true);
    runJsonReader(getBool(), js1) |> println;
    0


@test
def test04(): Int32 & Impure = 
    use NativeJson/MarshalJson.toJsonValue;
    use NativeJson/JsonBuilder.{createObject, putField, arrayOf, objectOf};
    let record1 = { name = "Flix", year = 2016, paradigms = "functional" :: "logic" :: "imperative" :: Nil };
    let record2 = { name = "Haskell", year = 1990, paradigms = "functional" :: "lazy" :: Nil };
    let writer = x -> 
        createObject()
            |> putField("name", x.name)
            |> putField("year", x.year)
            |> putField("paradigms", arrayOf(x.paradigms));
    arrayOf(List.map(writer >> objectOf, record1 :: record2 :: Nil)) |> NativeJson/Pretty.renderJson |> println;
    0

}
