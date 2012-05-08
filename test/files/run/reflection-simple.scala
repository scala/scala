// a.scala
// Wed May  2 01:01:22 PDT 2012

object Test  {
  def main(args: Array[String]) {
    System.out.println("Running")
    case class Foo(a: Int, b: Int, c: Int)
    val props = reflect.mirror.classToType(classOf[Foo]).members.filter(_.isTerm).map(_.toString)
    props.toList.sorted foreach System.out.println
  }
}
