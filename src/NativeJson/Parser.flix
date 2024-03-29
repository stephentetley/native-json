/*
 * Copyright 2021 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


namespace NativeJson/Parser {

    use Functor.{ignore};
    use Functor.{<$>, <$$>, $>};
    use Applicative.{point};

    use Basis/NIO/File.Path;

    use FlixParsec.Parser;
    use FlixParsec/Prim.{flatMap};
    use FlixParsec/Combinators.{choice, alt};
    use FlixParsec/TokenSimple.{symbol, lexeme, commaSep, commaSepWith, 
        colon, float64, int64};

    use NativeJson.JsonValue;
    use NativeJson.JsonValue.{JsonObject, JsonArray, JsonString, 
        JsonFloat, JsonInt, JsonBool, JsonNull};

    /// JSON is always UTF-8...


    pub def parseJsonString(s: String): Result[JsonValue, String] =
        FlixParsec/Prim.parsesString(pValue(), s)


    pub def parseJsonFile(tabSize: Int32, path: Path): Result[JsonValue, String] & Impure =
        FlixParsec/Extras.parsesFile(pValue(), tabSize, path, Basis/NIO/Charset/Charset.utf_8())


    pub def pValue(): Parser[t1, JsonValue] = pValue2()

    def pValue2(): Parser[t1, JsonValue] = 
        alt(JsonArray <$> pArray(), pValue1())

    def pValue1(): Parser[t1, JsonValue] = 
        alt(JsonObject <$> pObject(), basicTerm())

    def basicTerm(): Parser[t1, JsonValue] =  
        choice( 
               (pString()   <$$>    JsonString)
            :: (pFloat()    <$$>    JsonFloat)
            :: (pInt()      <$$>    JsonInt)
            :: (pTrue()     $>      JsonBool(true))
            :: (pFalse()    $>      JsonBool(false))
            :: (pNull()     $>      JsonNull)
            :: Nil
        )


    pub def pNull(): Parser[t1, Unit] = 
        lexeme(symbol("null")) |> ignore


    pub def pTrue(): Parser[t1, Unit] = 
        lexeme(symbol("true")) |> ignore

    pub def pFalse(): Parser[t1, Unit] = 
        lexeme(symbol("false")) |> ignore

    pub def pArray(): Parser[t1, List[JsonValue]] = 
        let* _  = symbol("[");
        let* xs = commaSep(pValue());
        let* _  = symbol("]");
        point(xs)

    /// Needs rewriting to follow Json rules...
    pub def pString(): Parser[t1, String] = 
        FlixParsec/TokenSimple.stringLiteral()

    pub def pObject(): Parser[t1, Map[String, JsonValue]] = 
        let step = (ac, xy) -> {let (k, v) = xy; Map.insert(k, v, ac)};
        let* _      = symbol("{");
        let* kvs    = commaSepWith(step, Map.empty(), pField());
        let* _      = symbol("}");
        point(kvs)


    def pField(): Parser[t1, (String, JsonValue)] = 
        let* n  = pString();
        let* _  = colon();
        let* v  = pValue();
        point((n, v))

    /// Needs rewriting to follow Json rules...
    pub def pNumber(): Parser[t1, Float64] = 
        let pInt1 = Int64.toFloat64 <$> pInt(); 
        /// TODO should use int part as common prefix rather than alt...
        alt(pFloat(), pInt1)

    pub def pInt(): Parser[t1, Int64] = int64()

    pub def pFloat(): Parser[t1, Float64] = float64()


}
