

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running... ${Time/LocalTime.now()}");
    test01();
    temp01();
    test02();
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