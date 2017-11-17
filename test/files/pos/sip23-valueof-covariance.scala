object Test {
  trait Foo[+A]
  implicit def foo[A <: Singleton](implicit v: ValueOf[A]): Foo[A] = new Foo[A] { }
  val s = ""
  implicitly[ValueOf[s.type]] // works
  implicitly[Foo[s.type]] // doesn't work
}
