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
    let _ = Console.printLine("${printConfig(ans)}");
    Ok()

type alias Config = 
    { uxlTemplate: System.FilePath
    , aibHierarchy: ExcelTable
    , args: List[String]
    }

type alias ExcelTable = {file: System.FilePath, sheet: String}

def printConfig(x: Config): String = 
    "{uxlTemplate = '${x.uxlTemplate}', aibHierarchy = {file = '${x.aibHierarchy.file}', sheet = '${x.aibHierarchy.sheet}'}, args = ${x.args}}"


def decode(): JsonReader[Config] = 
    use BasicJson/JsonReader.{return, flatMap, withField, withArray, getString};
    let* x1 = withField("uxl_template", filepath());
    let* x2 = withField("aib_hierarchy_export", excelTable());
    let* x3 = withField("args", withArray(getString()));
    return({
          uxlTemplate = x1
        , aibHierarchy = x2
        , args = x3
    })



def filepath(): JsonReader[System.FilePath] = 
    use BasicJson/JsonReader.{map, getString};
    map(System/FilePath.new, getString())

def excelTable(): JsonReader[ExcelTable] = 
    use BasicJson/JsonReader.{withField, getString};
    use BasicJson/JsonReader.{<&>, <*>};
    ((x1, x2) -> {file = x1, sheet = x2})
        <&> withField("file", filepath())
        <*> withField("sheet", getString())

