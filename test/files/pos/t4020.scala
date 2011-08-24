class A {
    sealed trait Foo
}

object a1 extends A {
    case class Foo1(i: Int) extends Foo
}

object a2 extends A {
    case class Foo2(i: Int) extends Foo
}

class B {
    def mthd(foo: a2.Foo) = {
        foo match {
            case a2.Foo2(i) => i

            // Note: This case is impossible.  In fact, scalac
            // will (correctly) report an error if it is uncommented,
            // but a warning if it is commented.

            // case a1.Foo1(i) => i
        }
    }
}