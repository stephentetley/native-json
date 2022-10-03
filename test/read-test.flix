
namespace ReadTest {

    use Basis/NIO/File.Path;

    use NativeJson.JsValue;
    use NativeJson.JsObject;
    use NativeJson.JsonReader;
    
    /// Warning - hardcoded path

    @test    
    def test01(): Result[Unit, String] & Impure = 
        use Result.{flatMap};
        use NativeJson/JsonReader.{readJsonFile, getString};
        let* srcpath = Basis/NIO/File/Path.newPath("E:/coding/flix/basic-json/demo/sample.json");
        let* ans = readJsonFile(decode(), srcpath);
        let _    = println("${printConfig(ans)}");
        Ok()

    type alias Config = 
        { uxlTemplate = Path
        , aibHierarchy = ExcelTable
        , args = List[String]
        }

    type alias ExcelTable = 
        { file = Path
        , sheet = String
        }

    def printConfig(x: Config): String = 
        "{uxlTemplate = '${x.uxlTemplate}', aibHierarchy = {file = '${x.aibHierarchy.file}', sheet = '${x.aibHierarchy.sheet}'}, args = ${x.args}}"


    def decode(): JsonReader[JsValue, Config] = 
        use NativeJson/JsonReader.{point, flatMap, getObjectWith, getFieldWith, getArrayWith, getString};
        getObjectWith(
            let* x1 = getFieldWith("uxl_template", NativeJson/JsonReader.filePath());
            let* x2 = getFieldWith("aib_hierarchy_export", excelTable());
            let* x3 = getFieldWith("args", getArrayWith(getString()));
            // let* _ = getString();
            point({
                uxlTemplate = x1
                , aibHierarchy = x2
                , args = x3
            }))



    def excelTable(): JsonReader[JsValue, ExcelTable] = 
        use NativeJson/JsonReader.{getObjectWith, getFieldWith, getString};
        use Functor.{<$>};
        use Applicative.{<*>};
        getObjectWith(
        ((x1, x2) -> {file = x1, sheet = x2})
            <$> getFieldWith("file", NativeJson/JsonReader.filePath())
            <*> getFieldWith("sheet", getString())
        )

}
