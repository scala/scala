package scala.tools.nsc.util

object trace {
  def apply[T](msg: String)(value: T): T = {
    println(msg+value)
    value
  }
  def withFun[T, U](msg: String)(value: T)(fun: T => U): T = {
    println(msg+fun(value))
    value
  }
}
