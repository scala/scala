package scala.tools.nsc
package util

import java.io.PrintStream

object trace extends SimpleTracer(System.out)
object errtrace extends SimpleTracer(System.err)

class SimpleTracer(out: PrintStream) {
    def apply[T](msg: String)(value: T): T = {
    out.println(msg+value)
    value
  }
  def withFun[T, U](msg: String)(value: T)(fun: T => U): T = {
    out.println(msg+fun(value))
    value
  }
}
