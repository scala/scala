final class Option[+A](val value: A) extends AnyVal

abstract class Foo1[A]                         { def f(): A }
         class Bar1[A] extends Foo1[Option[A]] { def f(): Option[A] = ??? }
