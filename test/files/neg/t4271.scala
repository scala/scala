object foo {
  object Donotuseme
  implicit def any2Ensuring[A](x: A) = Donotuseme
  implicit def doubleWrapper(x: Int) = Donotuseme
  implicit def floatWrapper(x: Int) = Donotuseme
  implicit def intWrapper(x: Int) = Donotuseme
  implicit def longWrapper(x: Int) = Donotuseme
  implicit def any2ArrowAssoc[A](x: A) = Donotuseme
  3 to 5
  5 ensuring true
  3 -> 5
}
