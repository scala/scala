import scala.language.experimental.macros

import scala.reflect.runtime.universe.{TypeTag => TT}
import scala.reflect.macros.blackbox.Context

trait TC[T]

object TC {
  implicit def instance[T]: TC[T] = macro instanceImpl[T]

  def instanceImpl[T: c.WeakTypeTag](c: Context): c.Expr[TC[T]] = {
    import c.universe._
    val wtt: c.WeakTypeTag[T] = implicitly[c.WeakTypeTag[T]]

    if (wtt.tpe.typeSymbol.isClass) {
      println(s"Known Subclasses for ${wtt.tpe.toString} ${wtt.tpe.typeSymbol.asClass.knownDirectSubclasses.toString}")
    }

    reify(new TC[T]{})
  }
}
