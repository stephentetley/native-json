
namespace ReadTest {

    use Interop/System.FilePath;

    use BasicJson.JsValue;
    use BasicJson.JsObject;
    use BasicJson.JsonReader;
    
    /// Warning - hardcoded path

    @test    
    def test01(): Result[Unit, String] & Impure = 
        use Result.{flatMap};
        use BasicJson/JsonReader.{readJsonFile, getString};
        let srcpath = Interop/System/FilePath.filePath("E:/coding/flix/basic-json/demo/sample.json");
        let* ans = readJsonFile(decode(), srcpath);
        let _    = println("${printConfig(ans)}");
        Ok()

    type alias Config = 
        { uxlTemplate :: FilePath
        , aibHierarchy :: ExcelTable
        , args :: List[String]
        }

    type alias ExcelTable = 
        { file :: FilePath
        , sheet :: String
        }

    def printConfig(x: Config): String = 
        "{uxlTemplate = '${x.uxlTemplate}', aibHierarchy = {file = '${x.aibHierarchy.file}', sheet = '${x.aibHierarchy.sheet}'}, args = ${x.args}}"


    def decode(): JsonReader[JsValue, Config] = 
        use BasicJson/JsonReader.{point, flatMap, getObjectWith, getFieldWith, getArrayWith, getString};
        getObjectWith(
            let* x1 = getFieldWith("uxl_template", filepath());
            let* x2 = getFieldWith("aib_hierarchy_export", excelTable());
            let* x3 = getFieldWith("args", getArrayWith(getString()));
            // let* _ = getString();
            point({
                uxlTemplate = x1
                , aibHierarchy = x2
                , args = x3
            }))



    def filepath(): JsonReader[JsValue, FilePath] = 
        use BasicJson/JsonReader.{map, getString};
        map(Interop/System/FilePath.filePath, getString())

    def excelTable(): JsonReader[JsValue, ExcelTable] = 
        use BasicJson/JsonReader.{getObjectWith, getFieldWith, getString};
        use Functor.{<$>};
        use Applicative.{<*>};
        getObjectWith(
        ((x1, x2) -> {file = x1, sheet = x2})
            <$> getFieldWith("file", filepath())
            <*> getFieldWith("sheet", getString())
        )

}