
import language._

object Sample {
  trait X
  trait Y

  // import of the non-implicit should be unused
  object Implicits {
    def `int to X`(i: Int): X = null
    implicit def `int to Y`(i: Int): Y = null
    implicit def useless(i: Int): String = null
  }

  def f(x: X) = ???
  def g(y: Y) = ???
}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
 
object Macro {
  def f: Int = macro fImpl
  def fImpl(c: Context): c.Tree = {
    import c.universe._
 
    q"""
     import scala.util.Random
     42 // TODO randomize
    """
  }
}
