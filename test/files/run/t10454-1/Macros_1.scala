import scala.language.experimental.macros

import scala.reflect.macros.blackbox.Context

class TC[T](val subclasses: List[String])

object TC {
  implicit def instance[T]: TC[T] = macro instanceImpl[T]

  def instanceImpl[T](c: Context)(implicit tT: c.WeakTypeTag[T]): c.Tree = {
    import c.universe._

    val subclasses =
      if (tT.tpe.typeSymbol.isClass)
        tT.tpe.typeSymbol.asClass.knownDirectSubclasses.map(_.toString).toList
      else Nil

    q"""new TC[$tT]($subclasses)"""
  }
}
