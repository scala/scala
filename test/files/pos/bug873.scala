abstract class Foo {

  val x:Option[List[String]]
  val y:List[Int]

  val z = (0:Any) match {
    case 1 => x
    case 2 => y
  }
}
