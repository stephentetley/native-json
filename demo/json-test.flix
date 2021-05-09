

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running...");
    test01();
    temp01();
    0

def test01(): Int32 & Impure =
    use BasicJson/Pretty.{ppBoolean, ppInt32, ppString};
    ppInt32(1000) |> println;
    ppString(String.unlines("Hello" :: "World" :: Nil)) |> println;
    0

def temp01(): Int32 & Impure =     
    let slash = '\\';
    Console.printLine("${slash}");
    Console.printLine("${Char.toInt32(slash)}");
    0