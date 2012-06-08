// a.scala
// Wed May  2 01:01:22 PDT 2012

object Test  {
  def main(args: Array[String]) {
    System.out.println("Running")
    case class Foo(a: Int, b: Int, c: Int)
    import scala.reflect.runtime.{currentMirror => cm}
    val props = cm.classSymbol(classOf[Foo]).typeSignature.members.filter(_.isTerm).map(_.toString)
    props.toList.sorted foreach System.out.println
  }
}
