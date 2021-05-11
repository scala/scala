class A {
  def aaa: Int = 10
}
class B extends A {
  private[this] def aaa: Int = 42
}
