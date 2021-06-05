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
    use Text/PrettyPrint.{text, char, hcat, dquotes, float64, int64, colon, 
        commaBrackets, intersperse, comma, hang, lbrace, rbrace, lbracket, rbracket};
    use Text/PrettyPrint.{<>, <<>>, <&&>};

    use BasicJson/Internal.StringWriter;
    use BasicJson.JsonValue;
    use BasicJson.JsonValue.{JsonObject, JsonArray, JsonString, 
        JsonFloat, JsonInt, JsonBool, JsonNull};
    
    pub def writeJson(path: System.FilePath, value: JsonValue): Result[Unit, String] & Impure =
        Text/PrettyPrint.writePretty(160, ToString.toString(path), Text/Charset.utf_8(), ppValue(value))


    pub def ppValue(x: JsonValue): Doc = match x {
        case JsonObject(nvs) => ppObject(nvs)
        case JsonArray(xs) => ppArray(xs)
        case JsonString(s) => ppString(s)
        case JsonFloat(d) => ppFloat(d)
        case JsonInt(d) => ppInt(d)
        case JsonBool(b) => ppBoolean(b)
        case JsonNull => ppNull()
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

    pub def ppArray(xs: List[JsonValue]): Doc = 
        let elements = xs |> List.map(ppValue) |> intersperse(comma()); 
        hang(2, (lbracket() <&&> elements)) <&&> rbracket()
        

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
        import java.lang.Character:isISOControl(Char);
        isISOControl(c) as & Pure

    pub def ppObject(kvs: Map[String, JsonValue]): Doc = 
        let ppField = kv -> {let (k, v) = kv; ppString(k) <> colon() <<>> ppValue(v)};
        let fields = Map.toList(kvs) |> List.map(ppField) |> intersperse(comma()); 
        hang(2, (lbrace() <&&> fields)) <&&> rbrace()

}
