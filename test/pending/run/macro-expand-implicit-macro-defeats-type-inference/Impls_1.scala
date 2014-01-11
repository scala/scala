import scala.reflect.macros.whitebox.Context

object Impls {
  def foo[T: c.WeakTypeTag](c: Context): c.Expr[List[T]] = c.universe.reify {
    println("openImplicits are: " + c.literal(c.openImplicits.toString).splice)
    println("enclosingImplicits are: " + c.literal(c.enclosingImplicits.toString).splice)
    println("typetag is: " + c.literal(c.tag[T].toString).splice)
    null
  }
}