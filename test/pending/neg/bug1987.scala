

// Copyright Shunsuke Sogame 2008-2009.
// Distributed under the terms of an MIT-style license.


package object overloading {
    def bar(f: (Int) => Unit): Unit = ()
    def bar(f: (Int, Int) => Unit): Unit = ()
}

class PackageObjectOverloadingTest {
    overloading.bar( (i: Int) => () ) // doesn't compile.
}
