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

/// Experiment with a monadic wrapper over StringBuilder

/// TODO not stack safe - needs codensity transformation

namespace BasicJson/Internal {
    
    /// Impure because the StringBuilder is mutated
    pub enum StringWriter[r: Region, a: Type](StringBuilder[r] -> a)

    instance Functor[StringWriter[r]] {
        pub def map(f: a -> b & ef, ma: StringWriter[r, a]): StringWriter[r, b] & ef = 
            BasicJson/Internal/StringWriter.map(f, ma)
    }

    instance Applicative[StringWriter[r]] {
        pub def point(x: a): StringWriter[r, a] = BasicJson/Internal/StringWriter.point(x)

        pub def ap(mf: StringWriter[r, a -> b & ef], ma: StringWriter[r, a]): StringWriter[r, b] & ef = 
            BasicJson/Internal/StringWriter.ap(mf, ma)
    }

    instance Monad[StringWriter[r]] {
        pub def flatMap(f: a -> StringWriter[r, b] & ef, ma: StringWriter[r, a]): StringWriter[r, b] & ef =
            BasicJson/Internal/StringWriter.flatMap(f, ma)
    }

}

namespace BasicJson/Internal/StringWriter {

    use Applicative.{*>};

    use BasicJson/Internal.StringWriter;
    use BasicJson/Internal.StringWriter.{StringWriter};

    pub def run(ma: StringWriter[r, a], r: Region[r]): (String, a) \ Write(r) = 
        let sb = new StringBuilder(r);
        let ans = apply1(ma, sb) as \ Write(r);
        let s = StringBuilder.toString(sb);
        (s, ans) 

    pub def exec(ma: StringWriter[r, a], r: Region[r]): String \ Write(r) = run(ma, r) |> fst

    def apply1(ma: StringWriter[r, a], sb: StringBuilder[r]): a = 
        let StringWriter(f) = ma;
        f(sb)


    pub def point(x: a): StringWriter[r, a] = StringWriter(_ -> x)

    /// Needs to allow allow effects (e.g create / access arrays)
    pub def map(fn: a -> b & ef, ma: StringWriter[r, a]): StringWriter[r, b] & ef = 
        StringWriter(sb -> 
            let a = apply1(ma, sb);
            fn(a) as & Pure
        ) as & ef

    pub def ap(mf: StringWriter[r, a -> b & ef], ma: StringWriter[r, a]): StringWriter[r, b] & ef = 
        StringWriter(sb -> 
            let f = apply1(mf, sb);
            let a = apply1(ma, sb);
            f(a) as & Pure
        ) as & ef

    pub def bind(ma: StringWriter[r, a], fn: a -> StringWriter[r, b] & ef): StringWriter[r, b] & ef= 
        StringWriter(sb -> 
            let a = apply1(ma, sb);
            apply1(fn(a) as & Pure, sb)
        ) as & ef

    pub def flatMap(fn: a -> StringWriter[r, b] & ef, ma: StringWriter[r, a]): StringWriter[r, b] & ef = bind(ma, fn)


    pub def writeString(s: String): StringWriter[r, Unit] \ Write(r) = 
        StringWriter(sb -> StringBuilder.append!(s, sb) as & Pure) as \ Write(r)

    pub def writeChar(c: Char): StringWriter[r, Unit] \ Write(r) = 
        StringWriter(sb -> StringBuilder.append!(c, sb) as & Pure) as \ Write(r)

    pub def mapX(f: a -> StringWriter[r, b] & ef, xs: List[a]): StringWriter[r, Unit] & ef = 
        StringWriter(sb -> 
            mapXHelper(sb, f, xs) as & Pure
        ) as & ef

    def mapXHelper(sb: StringBuilder[r], f: a -> StringWriter[r, b] & ef, xs: List[a]): Unit & ef = 
        match xs {
            case x :: rs => {
                let ma = f(x);
                let _  = apply1(ma, sb);
                mapXHelper(sb, f, rs)
            }
            case Nil => ()
        }

    pub def writes(x: a): StringWriter[r, Unit] \ Write(r) with ToString[a] = 
        ToString.toString(x) |> writeString

    pub def writesList(xs: List[a]): StringWriter[r, Unit] \ Write(r) with ToString[a] = 
        let* _ = mapX(x -> writes(x) *> writeString(" :: "), xs);
        writeString("Nil")
        
    pub def writesMap(kvs: Map[k, v]): StringWriter[r, Unit] \ Write(r) with ToString[k], ToString[v]= 
        let* _ = writeString("Map#{");
        let* _ = writesKeyValues(Map.toList(kvs));
        writeString("}")

    def writesKeyValues(kvs: List[(k, v)]): StringWriter[r, Unit] \ Write(r) with ToString[k], ToString[v] = match kvs {
        case Nil => point()
        case x :: rs => {
            let* _ = writesKV(x);
            mapX(xy -> writeString(", ") *> writesKV(xy), rs)
        }
    }

    def writesKV(kv: (k, v)): StringWriter[r, Unit] \ Write(r) with ToString[k], ToString[v]= 
        let (k, v) = kv;
        let* _ = writes(k);
        let* _ = writeString(" -> ");
        let* _ = writes(v);
        point()

    pub def writeParens(parens: Bool, ma: StringWriter[r, a]): StringWriter[r, a] \ Write(r) = 
        if (parens) {
            let* _  = writeChar('(');
            let* a = ma;
            let* _  = writeChar(')');
            point(a)
        } 
        else ma

            

}
