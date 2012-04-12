object Test extends App {
  import scala.reflect.mirror._
  val tree = Apply(Select(Ident("Macros"), newTermName("foo")), List(Literal(Constant(40))))
  try tree.eval
  catch { case ex: Throwable =>  println(ex.getMessage) }
}
