import scala.language.experimental.macros

package object tastytest {

  import scala.util.Random
  import scala.reflect.macros.blackbox.Context

  implicit final class SafeEq[T](private val t: T) extends AnyVal {
    final def ===[U](u: U)(implicit ev: T =:= U): Boolean = ???
  }

  def compiletimeHasChild[T](child: String): Unit = macro Macros.hasChildImpl[T]
  def compiletimeHasNestedChildren[T](children: String*): Unit = macro Macros.hasChildrenImpl[T]

  object Macros {

    def hasChildrenImpl[T](c: Context)(children: c.Expr[String]*)(implicit T: c.WeakTypeTag[T]): c.Expr[Unit] = {
      import c.universe._

      def findChildren(sym: Symbol): Set[Symbol] =
        sym.asClass.knownDirectSubclasses.flatMap(s => findChildren(s) + s)

      val sym = T.tpe.typeSymbol
      if (!sym.isClass) {
        c.error(c.enclosingPosition, s"${T.tpe} is not a class type; cannot inspect sealed children")
      }
      else {
        children.foreach { child =>
          child.tree match {
            case Literal(Constant(nmeString: String)) =>
              val children = findChildren(sym)
              val contains = children.toList.map(_.fullName).exists(_ == nmeString)
              if (!contains) {
                c.error(child.tree.pos, s"$sym does not have a child symbol $nmeString")
              }
            case _ =>
              c.error(child.tree.pos, s"$child is not constant string")
          }
        }
      }
      c.Expr[Unit](q"()")
    }

    def hasChildImpl[T](c: Context)(child: c.Expr[String])(implicit T: c.WeakTypeTag[T]): c.Expr[Unit] =
      hasChildrenImpl(c)(child)
  }

  def getRandomNat: Int = ???

  def getRandomPos: Int = ???
}
