package test;

object test {
    class A
    class B

    def m(a: A,  b: B*) = b.toArray
    def m(a: A*, b: B ) = a.toArray
    def m(a: A*, b: B*) = a.toArray
}
