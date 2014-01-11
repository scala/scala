package scala.collection.slick

object QueryableMacros{
  def map[T:c.WeakTypeTag, S:c.WeakTypeTag]
         (c: scala.reflect.macros.blackbox.Context)
         (projection: c.Expr[T => S])
         : c.Expr[scala.collection.slick.Queryable[S]] = {
    import c.universe._
    val code = EmptyTree
    c.universe.reify{
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