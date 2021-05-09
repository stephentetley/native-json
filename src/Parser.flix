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


namespace BasicJson/Parser {

    use FlixParsec.Parser;
    use FlixParsec/Prim.{flatMap, return, map};
    use FlixParsec/Combinators.{choice};
    use FlixParsec/Combinators.{*>};
    use FlixParsec/TokenSimple.{symbol, lexeme};

    pub def pBoolean(): Parser[t1, Bool] = 
        lexeme(
            choice( 
                    (symbol("true")  *> return(true))
                ::  (symbol("false") *> return(false))
                :: Nil
            )
        )

    /// pub def pString(): Parser[t1, String] = 
    ///     let* _ = char('"');



}
