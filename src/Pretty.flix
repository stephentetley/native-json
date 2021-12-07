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


namespace BasicJson/Pretty {

    use Text/PrettyPrint.Doc;
    use Text/PrettyPrint.{text, char, dquotes, float64, int64, colon, 
        intersperse, comma, hang, group, lbrace, rbrace, lbracket, rbracket, empty};
    use Text/PrettyPrint.{<>, <<>>, <&&>, <!>};

    use BasicJson/Internal.StringWriter;
    use BasicJson.JsonValue;
    use BasicJson.JsonValue.{JsonObject, JsonArray, JsonString, 
        JsonFloat, JsonInt, JsonBool, JsonNull};
    
    pub def writeJson(path: System.FilePath, value: JsonValue): Result[Unit, String] & Impure =
        let _ = Console.printLine("ppValue(_) ... ${Time/LocalTime.now()}");
        let doc = ppJson(value);
        let _ = Console.printLine("Text/PPrint.writePretty(_) ... ${Time/LocalTime.now()}");
        Text/PrettyPrint.writePretty(160, path, Text/Charset.utf_8(), doc)

    pub def renderJson(value: JsonValue): String =
        let doc = ppJson(value);
        Text/PrettyPrint.render(160, doc)



    pub def ppJson(x: JsonValue): Doc = ppValue(x, d1 -> d1)

    def ppValue(x: JsonValue, cont: Doc -> Doc): Doc = match x {
        case JsonObject(nvs) => ppObject(nvs, cont)
        case JsonArray(xs) => ppArray(xs, cont)
        case JsonString(s) => cont(ppString(s))
        case JsonFloat(d) => cont(ppFloat(d))
        case JsonInt(d) => cont(ppInt(d))
        case JsonBool(b) => cont(ppBoolean(b))
        case JsonNull => cont(ppNull())
    }


    pub def ppNull(): Doc = text("null")

    pub def ppBoolean(b: Bool): Doc = 
        if (b) text("true") else text("false")


    pub def ppFloat(d: Float64): Doc = 
        match d {
            case x if Float64.isNan(x) => ppString("NaN")
            case x if not Float64.isFinite(x) => ppString("Infinity")
            case _ => float64(d)
        }

    pub def ppInt(n: Int64): Doc = int64(n)


    /// TODO escapes, multiline etc.
    pub def ppString(s: String): Doc = 
        use BasicJson/Internal/StringWriter.{exec, mapX}; 
        let cs = String.toList(s);
        let s1 = exec(mapX(appendChar, cs));
        dquotes(text(s1))       /// TODO - check text is neutral with regards to escape chars


    def appendChar(c: Char): StringWriter[Unit] = 
        use BasicJson/Internal/StringWriter.{writeString, writeChar, seqRight};
        use BasicJson/Internal/StringWriter.{*>};
        use BasicJson/Internal/Utils.{int32ToHexString};
        let toHex = ch -> Char.toInt32(ch) |> int32ToHexString |> String.padLeft(4, '0');
        match c { 
            case '\\' => writeString("\\\\")
            case '/' => writeString("\\/")
            case '"' => writeChar('\\') *> writeChar('"')
            case cc if isISOControl(cc) => writeString("\\u${toHex(cc)}")
            case _ => writeChar(c)
        }


    def isISOControl(c: Char): Bool = 
        import static java.lang.Character.isISOControl(Char);
        isISOControl(c) as & Pure

    def ppObject(kvs: Map[String, JsonValue], cont: Doc -> Doc): Doc = 
        match Map.toList(kvs) {
            case Nil => cont(lbrace() <!> rbrace())
            case x1 :: rs => ppField(x1, v1 -> ppFieldsRest(rs, vs -> cont(hang(2, group(lbrace() <!> v1) <> vs <!> rbrace()))))
        }
    
    /// One field aready printed so start with a comma...
    def ppFieldsRest(xs: List[(String, JsonValue)], cont: Doc -> Doc): Doc = match xs { 
        case Nil => cont(empty())
        case x1 :: Nil => ppField(x1, v1 -> cont(comma() <!> v1))
        case x1 :: rs => 
            ppField(x1, v1 -> 
                ppFieldsRest(rs, vs -> 
                    cont(comma() <!> v1  <> vs)))
    }

    def ppField(kv: (String, JsonValue), cont: Doc -> Doc): Doc =
        let (key, value) = kv;
        ppValue(value, v1 -> {
            let d1 = ppString(key) <> colon() <<>> v1;
            cont(d1)})

    def ppArray(xs: List[JsonValue], cont: Doc -> Doc): Doc = match xs { 
        case Nil => cont(lbracket() <> rbracket())
        case x1 :: rs => 
            ppValue(x1, v1 -> 
                ppArrayRest(rs, vs -> cont(hang(2, (group(lbracket() <!> v1) <!> vs <!> rbracket())))))
    }

    /// One field aready printed so start with a comma...
    def ppArrayRest(xs: List[JsonValue], cont: Doc -> Doc): Doc = match xs { 
        case Nil => cont(empty())
        case x1 :: Nil => ppValue(x1, v1 -> cont(comma() <!> v1))
        case x1 :: rs => 
            ppValue(x1, v1 -> 
                ppArrayRest(rs, vs -> 
                    cont(comma() <!>  v1 <> vs)))
    }

}
