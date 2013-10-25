// get expected error message without package declaration
package ex

import scala.language.experimental.macros
import scala.reflect.macros._

object IW {
  def foo(a: String): String = ???
}
object Mac {
  def mac(s: String): String = macro macImpl
  def macImpl(c: Context)(s: c.Expr[String]): c.Expr[String] = 
    c.universe.reify(IW.foo(s.splice))
}
