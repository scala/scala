package scala.collection.slick
object QueryableMacros{
  def map[T:c.TypeTag, S:c.TypeTag]
         (c: scala.reflect.makro.Context)
         (projection: c.mirror.Expr[T => S])
         : c.mirror.Expr[scala.collection.slick.Queryable[S]] = {
    import c.mirror._
    val code = EmptyTree
    c.reify{
      Queryable.factory[S]( code.asInstanceOf[reflect.mirror.Tree] )
    }
  }
}
class Queryable[T]{
  def map[S]( projection: T => S ) : Queryable[S] = macro QueryableMacros.map[T,S]
}
object Queryable{
  def factory[S]( projection:scala.reflect.mirror.Tree ) : Queryable[S] = null
}