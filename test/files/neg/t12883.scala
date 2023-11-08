//> using options -Xsource:3

case class C private (c: Int) {
  def copy(c: Int = this.c): C = new C(c)
}
