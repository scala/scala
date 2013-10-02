class A { def foo[A](a: A) = a }
class B extends A { override def foo[A](b) = b }
