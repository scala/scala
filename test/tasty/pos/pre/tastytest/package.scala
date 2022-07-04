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

  /** forces annotations of type `A` on methods from class `T` */
  def forceAnnots[T, A, S <: String with Singleton]: Unit = macro Macros.AnnotsBundle.forceAnnotsImpl[T, A, S]

  object Macros {

    class AnnotsBundle(val c: Context) {
      import c.universe._

      private def annotType(annot: Annotation): Type = annot.tree.tpe match {
        case TypeBounds(lo, hi) => hi
        case tpe => tpe
      }

      private def toExplore[T](implicit T: c.WeakTypeTag[T]): List[Symbol] = (
        weakTypeOf[T].typeSymbol
        +: weakTypeOf[T].typeSymbol.asInstanceOf[ClassSymbol].primaryConstructor
        +: weakTypeOf[T].members.filter(_.isMethod).toList.flatMap(method =>
          method :: method.asInstanceOf[MethodSymbol].paramLists.flatten
        )
      )

      private def stringAssert[S <: String with Singleton](implicit S: c.WeakTypeTag[S]): String =
        weakTypeOf[S] match {
          case ConstantType(Constant(str: String)) => str
          case _ => ???
        }

      def forceAnnotsImpl[T, A, S <: String with Singleton](implicit T: c.WeakTypeTag[T], A: c.WeakTypeTag[A], S: c.WeakTypeTag[S]): c.Expr[Unit] = {
        val trees = {
          for {
            defn <- toExplore[T]
            annot <- defn.annotations.filter(annotType(_).typeSymbol == weakTypeOf[A].typeSymbol)
          } yield {
            s"${annot.tree}"
          }
        }
        val annotStr = trees.head
        assert(annotStr == stringAssert[S], s"actually, was $annotStr")
        c.Expr[Unit](q"()")
      }
    }

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
