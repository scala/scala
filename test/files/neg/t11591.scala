object Test {
  class A
  class B

  implicit def mkA(implicit b: => B): A = ???
  implicit def mkB(implicit a: A, i: Int): B = ???

  implicitly[A]
}
