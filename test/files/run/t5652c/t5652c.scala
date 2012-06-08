class A1 {
  def f1 = { def g = 1 ; class A { def a = g } ; new A().a }
  def f2 = { def g = 2 ; class A { def a = g } ; new A().a }
}

object Test extends App {
  println(classOf[A1].getDeclaredMethods.map(_.toString).sorted.mkString("\n"))
  println(new A1().f1)
  println(new A1().f2)
}
