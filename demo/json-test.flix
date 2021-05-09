

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running...");
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

def test02(): Int32 & Impure =
    use BasicJson/Parser.{pTrue};
    FlixParsec/Prim.parsesString(pTrue(), "true") |> println;
    0