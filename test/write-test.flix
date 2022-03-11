
namespace WriteTest {


    use BasicJson.JsonWriter;
    

    @test    
    def test01(): Result[Unit, String] & Impure = 
        use BasicJson/Pretty.{renderJson};
        let (json, _) = BasicJson/JsonWriter.runJsonWriter(encode());
        let _    = println("${renderJson(json)}");
        Ok()



    def encode(): JsonWriter[ka, Unit] & Impure = 
        use BasicJson/JsonObjectWriter.{flatMap};
        use Applicative.point;
        use BasicJson/JsonWriter.{putArrayWith, putObjectWith, putFieldWith, putString, putValue};
        let putLang = (name, year) -> putObjectWith(
                let* _  = putFieldWith("name", putString(name));
                let* _  = putFieldWith("year", putValue(year));
                point()
            );
        putArrayWith(
            putLang("C99", 1999) :: putLang("SML97", 1997) :: putLang("SQL-92", 1992) :: Nil
        )


}