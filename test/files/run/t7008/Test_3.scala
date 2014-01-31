import scala.reflect.runtime.universe._

object Test extends App {
  Macros.foo
  println("=============")

  val decls = typeOf[JavaClassWithCheckedExceptions_1[_]].decls.toList
  decls sortBy (_.name.toString) foreach (decl => println(s"${decl.name}: ${decl.annotations}"))
}