
// scalac: --release 8 -opt:inline:** -Wopt -Werror
//
class C {
  val cv = Map[Int, Int](1 -> 2)
  lazy val cl = Map[Int, Int](1 -> 2)
  def cd = Map[Int, Int](1 -> 2)
  def f(s: String) = s.translateEscapes
}
