import scala.reflect.macros.Context
import language.experimental.macros

trait Complex[T]

class Foo

object Complex {
  def impl[T: c.WeakTypeTag](c: Context): c.Expr[Complex[T]] = {
    import c.universe._
    def shout(msg: String) = {
      val cannotShutMeUp = c.asInstanceOf[scala.reflect.macros.runtime.Context].universe.currentRun.currentUnit.error _
      cannotShutMeUp(c.enclosingPosition.asInstanceOf[scala.reflect.internal.util.Position], msg)
    }
    try {
      val complexOfT = appliedType(typeOf[Complex[_]], List(weakTypeOf[T]))
      val infiniteRecursion = c.inferImplicitValue(complexOfT, silent = true)
      shout("silent = true does work!")
    } catch {
      case ex: Exception => shout(ex.toString)
    }
    c.literalNull
  }

  implicit def genComplex[T]: Complex[T] = macro impl[T]
}
