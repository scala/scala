import scala.reflect.makro.Context

case class Utils[C <: Context]( c:C ) {
  import c.mirror._
  import c.{Tree=>_}
  object removeDoubleReify extends c.mirror.Transformer {
    def apply( tree:Tree ) = transform(tree)
    override def transform(tree: Tree): Tree = {
      super.transform {
        tree match {
          case  Apply(TypeApply(Select(_this, termname), _), reified::Nil )
                if termname.toString == "factory" => c.unreifyTree(reified)
          case Apply(Select(_this, termname), reified::Nil )
               if termname.toString == "factory" => c.unreifyTree(reified)
          case _ => tree
        }
      }
    }
  }
}
object QueryableMacros{
  def _helper[C <: Context,S:c.TypeTag]( c:C )( name:String, projection:c.mirror.Expr[_] ) = {
    import c.mirror._
    val element_type = implicitly[c.TypeTag[S]].tpe
    val foo = Expr[reflect.mirror.Expr[Queryable[S]]](
    c.reifyTree( c.reflectMirrorPrefix, c.typeCheck(
      Utils[c.type](c).removeDoubleReify(
        Apply(Select(c.prefix.tree, newTermName( name )), List( projection.tree ))
       ).asInstanceOf[Tree]
      )))
    c.reify{ Queryable.factory[S]( foo.eval )}
  }
  def map[T:c.TypeTag, S:c.TypeTag]
               (c: scala.reflect.makro.Context)
               (projection: c.mirror.Expr[T => S]): c.mirror.Expr[Queryable[S]] = _helper[c.type,S]( c )( "_map", projection )
}
class Queryable[T]{
  def _map[S]( projection: T => S ) : Queryable[S] = ???
  def map[S]( projection: T => S ) : Queryable[S] = macro QueryableMacros.map[T,S]
}
object Queryable{
  def factory[S]( projection:scala.reflect.mirror.Expr[Queryable[S]] ) : Queryable[S] = null
}