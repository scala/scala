//> using options -Werror -Xsource:3

case class C private (c: Int) {
  def copy(c: Int = this.c): C = new C(c)
}

object C {
  def apply(c: Int) = new C(c)
}
