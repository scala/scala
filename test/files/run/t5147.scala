class Test extends Enumeration {
  val A = Value
}
object Test extends Test
object Test5147 {
  def main(args: Array[String]): Unit = {
    println(Test.A)
  }
}
