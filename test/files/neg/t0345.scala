object Lizt {
    val empty = new Lizt[Nothing] {
        def cons[A](a : A) {}
    }
}

trait Lizt[A] {
    def cons(a : A) : Unit
}
class Test {
    abstract class C[A] {}
    val c = new C[Int] {
        def f[A](x: A) {}
    }
 }
