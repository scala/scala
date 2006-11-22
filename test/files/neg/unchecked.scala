abstract class Test {
  type T <: Option[String]
  val x: Option[String]
  def f {
    x match {
      case x: T => Console.println("4")
      case a: Some[String] => Console.println("3")
      case None => Console.println("else case")
    }
  }
}
