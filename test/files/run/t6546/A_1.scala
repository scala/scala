final class Opt {
  @inline def getOrElse(x: => String): String = ""
}
class A_1 {
  def f(x: Opt): String = x getOrElse null
}
