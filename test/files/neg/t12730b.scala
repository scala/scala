//> using options -Werror -Xlint
package p {
  @deprecated("package delivery", since="recently")
  object `package` {
    def m = "member"
  }
}

trait T {
  def f = p.m
}
