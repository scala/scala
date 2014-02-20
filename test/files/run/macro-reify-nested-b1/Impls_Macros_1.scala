import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

case class Utils[C <: Context]( c:C ) {
  import c.universe._
  import c.{Tree=>_}
  object removeDoubleReify extends c.universe.Transformer {
    def apply( tree:Tree ) = transform(tree)
    override def transform(tree: Tree): Tree = {
      super.transform {
        tree match {
          case  Apply(TypeApply(Select(_this, termname), _), reification::Nil )
                if termname.toString == "factory" => c.unreifyTree(reification)
          case Apply(Select(_this, termname), reification::Nil )
               if termname.toString == "factory" => c.unreifyTree(reification)
          case _ => tree
        }
      }
    }
  }
}
object QueryableMacros{
  def _helper[C <: Context,S:c.WeakTypeTag]( c:C )( name:String, projection:c.Expr[_] ) = {
    import c.universe._
    import internal._
    val element_type = implicitly[c.WeakTypeTag[S]].tpe
    val foo = c.Expr[ru.Expr[Queryable[S]]](
    c.reifyTree( gen.mkRuntimeUniverseRef, EmptyTree, c.typecheck(
      Utils[c.type](c).removeDoubleReify(
        Apply(Select(c.prefix.tree, TermName( name )), List( projection.tree ))
       ).asInstanceOf[Tree]
      )))
    c.universe.reify{ Queryable.factory[S]( foo.splice )}
  }
  def map[T:c.WeakTypeTag, S:c.WeakTypeTag]
               (c: Context)
               (projection: c.Expr[T => S]): c.Expr[Queryable[S]] = _helper[c.type,S]( c )( "_map", projection )
}
class Queryable[T]{
  def _map[S]( projection: T => S ) : Queryable[S] = ???
  def map[S]( projection: T => S ) : Queryable[S] = macro QueryableMacros.map[T,S]
}
object Queryable{
  def factory[S]( projection:ru.Expr[Queryable[S]] ) : Queryable[S] = null
}