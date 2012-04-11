object Test extends App {
  import scala.reflect.mirror._
  val tree = Select(Ident("Macros"), newTermName("foo"))
  try tree.eval
  catch { case ex: Throwable =>  println(ex.getMessage) }
}