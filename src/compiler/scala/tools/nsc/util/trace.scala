package scala.tools.nsc.util

object trace {
  def apply[T](msg: String)(value: T): T = {
    println(msg+value)
    value
  }
}
