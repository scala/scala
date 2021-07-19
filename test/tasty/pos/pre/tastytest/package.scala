import scala.language.experimental.macros

package object tastytest {

  import scala.util.Random
  import scala.reflect.macros.blackbox.Context

  import scala.collection.mutable

  implicit final class SafeEq[T](private val t: T) extends AnyVal {
    final def ===[U](u: U)(implicit ev: T =:= U): Boolean = ???
  }

  def compiletimeHasChild[T](child: String): Unit = macro Macros.hasChildImpl[T]
  def compiletimeHasNestedChildren[T](expected: String*): Unit = macro Macros.hasChildrenImpl[T]

  object Macros {

    def hasChildrenImpl[T](c: Context)(expected: c.Expr[String]*)(implicit T: c.WeakTypeTag[T]): c.Expr[Unit] = {
      import c.universe._

      def findChildren(sym: Symbol): Set[Symbol] = {
        def findLvlN(explore: mutable.ArrayDeque[Symbol], seen: Set[Symbol]): Set[Symbol] = {
          if (explore.nonEmpty) {
            val (s, rest) = (explore.head, explore.dropInPlace(1))
            val lvlN = s.asClass.knownDirectSubclasses
            val unseen = lvlN -- seen
            if (unseen.nonEmpty) {
              findLvlN(rest ++= unseen, seen ++ unseen)
            } else {
              findLvlN(rest, seen)
            }
          }
          else {
            seen
          }
        }

        val lvl1 = sym.asClass.knownDirectSubclasses
        if (lvl1.isEmpty) lvl1
        else findLvlN(mutable.ArrayDeque.from(lvl1 - sym), lvl1)
      }

      val sym = T.tpe.typeSymbol
      lazy val children = findChildren(sym)
      if (!sym.isClass) {
        c.error(c.enclosingPosition, s"${T.tpe} is not a class type; cannot inspect sealed children")
      }
      else {
        expected.foreach { child =>
          child.tree match {
            case Literal(Constant(nmeString: String)) =>
              val contains = children.exists(_.fullName == nmeString)
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
