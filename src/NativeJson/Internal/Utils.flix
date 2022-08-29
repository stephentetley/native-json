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


namespace NativeJson/Internal/Utils {



    pub def int32ToHexString(n: Int32): String = 
        import static java.lang.Integer.toHexString(Int32): String & Pure;
        toHexString(n)

    pub def integerPart(d: Float64): Int64 = 
        import static java.lang.Double.valueOf(Float64): ##java.lang.Double & Pure;
        import java.lang.Double.longValue(): Int64 & Pure;
        longValue(valueOf(d))

}
