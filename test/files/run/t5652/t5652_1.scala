trait T1 {
  def f0 = { def g = 1 ; class A { def a = g } ; g ; new A().a }
}
class A1 {
  def f1 = { def g = 1 ; class A { def a = g } ; g ; new A().a }
}
