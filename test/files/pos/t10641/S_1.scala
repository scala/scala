
class S_1 {
  val j = new J_0
  import j.{ `_` => u, funkyJavaNameFactory => f }   // was: Wildcard import must be in last position
  val x = u
  def y = f
}
