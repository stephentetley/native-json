

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running... ${Time/LocalTime.now()}");
    test01();
    temp01();
    test02();
    test03();
    test04();
    0

def test01(): Int32 & Impure =
    use BasicJson/Pretty.{ppBoolean, ppNumber, ppString};
    ppNumber(false, 1000.0f64) |> println;
    ppNumber(true, 1000.0001f64) |> println;
    ppString(String.unlines("Hello" :: "World" :: Nil)) |> println;
    0

def temp01(): Int32 & Impure =     
    let slash = '\\';
    Console.printLine("${slash}");
    Console.printLine("${Char.toInt32(slash)}");
    0

def dquote(s: String): String = 
    Char.toString('\u0022') + s + Char.toString('\u0022')

def test02(): Int32 & Impure =
    use BasicJson/Parser.{pValue};
    FlixParsec/Prim.parsesString(pValue(), "true") |> println;
    FlixParsec/Prim.parsesString(pValue(), "100.75") |> println;
    FlixParsec/Prim.parsesString(pValue(), "[true, null, 1000, false]") |> println;
    let key1 = dquote("field1");
    let key2 = dquote("field2");
    FlixParsec/Prim.parsesString(pValue(), "${key1}") |> println;
    FlixParsec/Prim.parsesString(pValue(), "{${key1}: 1000, ${key2}: false}") |> println;
    FlixParsec/Prim.parsesString(pValue(), "{${key1}: [true, null, 1000, false], ${key2}: false}") |> println;
    0

def test03(): Int32 & Impure =
    use BasicJson/MarshalJson.toJsonValue;
    use BasicJson/JsonReader.{runReader, getBool};
    let js1 = toJsonValue(true);
    runReader(js1, getBool()) |> println;
    0



def test04(): Int32 & Impure = 
    use BasicJson/MarshalJson.toJsonValue;
    use BasicJson/JsonBuilder.{createObject, putField, arrayOf, objectOf};
    let record1 = { name = "Flix", year = 2016, paradigms = "functional" :: "logic" :: "imperative" :: Nil };
    let writer = x -> 
        createObject()
            |> putField("name", x.name)
            |> putField("year", x.year)
            |> putField("paradigms", arrayOf(x.paradigms));
    writer(record1) |> objectOf |> BasicJson/Pretty.ppValue |> println;
    0

