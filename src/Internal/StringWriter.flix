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

namespace BasicJson/Internal {
    
    pub opaque type StringWriter[a] = StringBuilder ~> a

}

namespace BasicJson/Internal/StringWriter {
    
    use BasicJson/Internal.StringWriter;
    use BasicJson/Internal.StringWriter.{StringWriter};

    pub def run(ma: StringWriter[a]): (String, a) = runHelper(ma) as & Pure 
    
    def runHelper(ma: StringWriter[a]): (String, a) & Impure = 
        let sb = StringBuilder.new();
        let ans = apply1(ma, sb);
        let s = StringBuilder.toString(sb);
        (s, ans)

    pub def exec(ma: StringWriter[a]): String = run(ma) |> fst

    def apply1(ma: StringWriter[a], sb: StringBuilder): a & Impure = 
        let StringWriter(f) = ma;
        f(sb)


    pub def return(x: a): StringWriter[a] = 
        StringWriter(_ -> x as & Impure)

    /// Needs to allow allow effects (e.g create / access arrays)
    pub def map(fn: a -> b & ef, ma: StringWriter[a]): StringWriter[b] = 
        StringWriter(sb -> 
            let a = apply1(ma, sb);
            fn(a) as & Impure
        )

    pub def ap(mf: StringWriter[a -> b & ef], ma: StringWriter[a]): StringWriter[b] = 
        StringWriter(sb -> 
            let f = apply1(mf, sb);
            let a = apply1(ma, sb);
            f(a) as & Impure
        )

    pub def seqLeft(ma: StringWriter[a], mb: StringWriter[b]): StringWriter[a] = 
        let* a = ma;
        let* _ = mb;
        return(a)

    pub def seqRight(ma: StringWriter[a], mb: StringWriter[b]): StringWriter[b] = 
        let* _ = ma;
        let* b = mb;
        return(b)


    pub def bind(ma: StringWriter[a], fn: a -> StringWriter[b]): StringWriter[b] = 
        StringWriter(sb -> 
            let a = apply1(ma, sb);
            apply1(fn(a), sb)
        )

    pub def flatMap(fn: a -> StringWriter[b], ma: StringWriter[a]): StringWriter[b] = bind(ma, fn)


    pub def appendString(s: String): StringWriter[Unit] = 
        StringWriter(sb -> StringBuilder.appendString!(sb, s))

    pub def appendChar(c: Char): StringWriter[Unit] = 
        StringWriter(sb -> StringBuilder.appendChar!(sb, c))

    pub def mapX(f: a -> StringWriter[b], xs: List[a]): StringWriter[Unit] = 
        StringWriter(sb -> 
            mapXHelper(sb, f, xs)
        )

    def mapXHelper(sb: StringBuilder, f: a -> StringWriter[b], xs: List[a]): Unit & Impure= 
        match xs {
            case x :: rs => {
                let ma = f(x);
                let _  = apply1(ma, sb);
                mapXHelper(sb, f, rs)
            }
            case Nil => ()
        }

}
