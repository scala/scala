package object overloading {
    def bar(f: (Int) => Unit): Unit = ()
    def bar(f: (Int, Int) => Unit): Unit = ()
}

class PackageObjectOverloadingTest {
    overloading.bar( (i: Int) => () ) // doesn't compile.
}
