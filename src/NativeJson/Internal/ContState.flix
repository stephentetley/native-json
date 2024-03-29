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



namespace NativeJson/Internal/ContState {

    ///
    /// ContState is the State monad implemented in CPS. 
    ///
    /// A simple implementation of the State monad bursts the stack when `mapAccumLeft` is run on large
    /// collections, so use ContState the CPS-State monad instead.
    ///
    /// We use an Impure continuation so we can internally cast the `ef` of operations and not lose
    /// impurity information.
    ///
    pub enum ContState[ka, s, a](s -> (a -> s -> ka \ IO) -> ka \ IO)

    ///
    /// Helper function for ContState's implementations of `map` and `ap`.
    ///
    def apply1(ma: ContState[ka, s, a], st: s, k: a -> s -> ka \ IO): ka \ IO =
        let ContState(f) = ma;
        f(st, k)

    ///
    /// Returns the result (new state and answer) of applying `ma` to the initial state `st`.
    ///
    pub def runContState(ma: ContState[(s, a), s, a], st: s): (s, a) \ IO =
        let ContState(f) = ma;
        let cont = (a, s) -> unsafe_cast (s, a) as \ IO;
        f(st, cont)


    instance Functor[ContState[ka, s]] {
        pub def map(f: a -> b \ ef, ma: ContState[ka, s, a]): ContState[ka, s, b] \ ef =
            unsafe_cast (ContState((s, k) ->
                apply1(ma, s, (a, s1) -> k(unsafe_cast f(a) as \ IO, s1))
            )) as \ ef
    }

    instance Applicative[ContState[ka, s]] {
        pub def point(x: a): ContState[ka, s, a] =
            ContState((s, k) -> k(x, s))

        pub def ap(mf: ContState[ka, s, a -> b \ ef], ma: ContState[ka, s, a]): ContState[ka, s, b] \ ef =
            unsafe_cast (ContState((s, k) ->
                apply1(mf, s, (f, s1) ->
                    apply1(ma, s1, (a, s2) ->
                        k(unsafe_cast f(a) as \ IO, s2)))
            )) as \ ef
    }

    instance Monad[ContState[ka, s]] {
        pub def flatMap(f: a -> ContState[ka, s, b] \ ef, ma: ContState[ka, s, a]): ContState[ka, s, b] \ ef =
            unsafe_cast (ContState((s, k) ->
                apply1(ma, s, (a, s1) ->
                    apply1(unsafe_cast f(a) as \ IO, s1, (b, s2) ->
                        k(b, s2)))
            )) as \ ef
    }

    /// update is tricky, we must update the state for the rest of the computation, are we sure this works...

    pub def update(f: s -> s \ ef): ContState[ka, s, Unit] \ ef = 
        unsafe_cast (ContState((s, k) -> 
            k((), unsafe_cast f(s) as \ IO)
        )) as \ ef

    pub def put(st: s): ContState[ka, s, Unit] = 
        ContState((_, k) -> 
            k((), st)
        )

    pub def get(): ContState[ka, s, s] = 
        ContState((s, k) -> 
            k(s, s)
        )

    pub def gets(proj: s -> a \ ef): ContState[ka, s, a] \ ef = 
        unsafe_cast (ContState((s, k) -> 
            k(unsafe_cast proj(s) as \ IO, s)
        )) as \ ef


}
