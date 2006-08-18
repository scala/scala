trait D {
  private val x = 1
  private object xxxx { Console.println(x) }
}
object Go extends D {
  def main(args : Array[String]) : Unit = {};
}
trait D2 {
  val x = 1
  object yyyy { Console.println(x) }
}
object Go2 extends D2 {
  def main(args : Array[String]) : Unit = {};
}
