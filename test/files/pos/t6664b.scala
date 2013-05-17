object T {
  def A(s: String): A = new A(3, s)
  def A(i: Int): A = A(i, "abc")
  case class A(i: Int, s: String)
}
