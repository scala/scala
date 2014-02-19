import language.experimental.macros
import language.implicitConversions
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.universe.Tree

class ReflectiveClosure[A, B](val tree: Tree, fn: A => B) extends (A => B) {
  def apply(x: A) = fn(x)
}

object ReflectiveClosure {
  implicit def reflectClosure[A, B](f: A => B): ReflectiveClosure[A, B] = macro Macros.reflectiveClosureImpl[A, B]
}

object Macros {
  def reflectiveClosureImpl[A, B](c: Context)(f: c.Expr[A => B]): c.Expr[ReflectiveClosure[A, B]] = {
    import c.universe._
    import internal._
    val u = gen.mkRuntimeUniverseRef
    val m = EmptyTree
    val tree = c.Expr[scala.reflect.runtime.universe.Tree](Select(c.reifyTree(u, m, f.tree), newTermName("tree")))
    c.universe.reify(new ReflectiveClosure(tree.splice, f.splice))
  }
}
