
trait TheOldCollegeTry {
  def f = try (1) + 1 finally ()

  def g = try { 1 } + 1 finally ()

  type H[A] = PartialFunction[Throwable, A]

  val myh: H[Int] = { case _: NullPointerException => ??? ; case t => 42 }
  def h = try ??? catch myh finally ()

  // a little weird
  def pf: H[Nothing] = try { case _: Throwable => ??? } finally ()

  // but not weirder than
  def tf = try (t: Throwable) => throw t finally ()

  // old jedi mind trick
//badcatch.scala:14: error: recursive value catchExpr1 needs type
//    try {} catch catchExpr1
//                 ^
  def badcatch = {
    def catchExpr1: PartialFunction[Throwable, Any] = ???
    try {} catch catchExpr1
  }
}
