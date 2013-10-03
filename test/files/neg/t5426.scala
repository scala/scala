class A {
  def f1 = Some(5) == 5
  def f2 = 5 == Some(5)

  val x1 = 5
  val x2 = Some(5)

  (x1 == x2)
  (x2 == x1)
}
