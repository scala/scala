import scala.xml._

class Foo {
  val elements: Seq[Node] = Nil
  val innerTransform: PartialFunction[Elem, String] = {
    case Elem(_, l: String, _, _, _ @ _*) if elements.exists(_.label == l) => 
      l
  }
}

