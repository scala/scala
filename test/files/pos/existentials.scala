class A {
  def f() = { case class Bob(); Bob }

  val quux0 = f()
  def quux1 = f()
  
  val bippy0 = f _
  def bippy1 = f _
}
