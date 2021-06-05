use BasicJson.JsonReader;


def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running... ${Time/LocalTime.now()}");
    test01() |> report;
    0

def report(r: Result[a, String]): Unit & Impure = 
    match r { 
        case Ok(_) => Console.printLine("Ok.")
        case Err(msg) => Console.printLine("Err: ${msg}")
    }
    
def test01(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    use BasicJson/JsonReader.{readJsonFile, getString};
    let srcpath = System/FilePath.new("E:/coding/flix/basic-json/demo/sample.json");
    let* ans = readJsonFile(decode(), srcpath);
    let _ = Console.printLine("${ans}");
    Ok()

def decode(): JsonReader[String] = 
    use BasicJson/JsonReader.{return, flatMap, withField, getString};
    let* s1 = withField("uxl_template", getString());
    return(s1)
