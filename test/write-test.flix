
namespace WriteTest {


    use NativeJson.JsonWriter;


    @test    
    def test01(): Result[Unit, String] & Impure = 
        use NativeJson/Pretty.{renderJson};
        let (json, _) = NativeJson/JsonWriter.runJsonWriter(encode());
        let _    = println("${renderJson(json)}");
        Ok()



    def encode(): JsonWriter[ka, Unit] & Impure = 
        use NativeJson/JsonWriter.{flatMap};
        use Applicative.point;
        use NativeJson/JsonWriter.{putArrayWith, putObjectWith, putFieldWith, putString, putValue};
        let putLang = (name, year) -> putObjectWith(
                let* _  = putFieldWith("name", putString(name));
                let* _  = putFieldWith("year", putValue(year));
                point()
            );
        putArrayWith(
            putLang("C99", 1999) :: putLang("SML97", 1997) :: putLang("SQL-92", 1992) :: Nil
        )


}