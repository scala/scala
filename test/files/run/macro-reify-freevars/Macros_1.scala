package scala.collection.slick

object QueryableMacros{
  def map[T:c.TypeTag, S:c.TypeTag]
         (c: scala.reflect.makro.Context)
         (projection: c.Expr[T => S])
         : c.Expr[scala.collection.slick.Queryable[S]] = {
    import c.universe._
    val code = EmptyTree
    c.reify{
      Queryable.factory[S]( code.asInstanceOf[reflect.runtime.universe.Tree] )
    }
  }
}
class Queryable[T]{
  def map[S]( projection: T => S ) : Queryable[S] = macro QueryableMacros.map[T,S]
}
object Queryable{
  def factory[S]( projection:reflect.runtime.universe.Tree ) : Queryable[S] = null
}