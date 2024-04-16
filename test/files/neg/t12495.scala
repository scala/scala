//> using options -Werror -Xlint:arg-discard,adapted-args -Wvalue-discard
class C {
  val f = (u: Unit) => println(s"[$u]")
  def g = f(42, 27)
}
