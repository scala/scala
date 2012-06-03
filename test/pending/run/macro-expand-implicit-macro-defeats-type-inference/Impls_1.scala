import scala.reflect.makro.Context

object Impls {
  def foo[T: c.TypeTag](c: Context): c.Expr[List[T]] = c.reify {
    println("openImplicits are: " + c.literal(c.openImplicits.toString).splice)
    println("enclosingImplicits are: " + c.literal(c.enclosingImplicits.toString).splice)
    println("typetag is: " + c.literal(c.tag[T].toString).splice)
    null
  }
}