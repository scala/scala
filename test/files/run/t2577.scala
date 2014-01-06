case class annot[T]() extends scala.annotation.StaticAnnotation

// type inference should infer @annot[Nothing] instead of @annot[T]
// note the T is not in scope here!
class Foo[@annot U]

object Test {
  import scala.reflect.runtime.universe._
  val x = new Foo

  def main(args: Array[String]): Unit = {
    val targ = typeOf[x.type].widen match {
      case TypeRef(_, _, arg :: _) => arg
    }
    println(targ)
  }
}
