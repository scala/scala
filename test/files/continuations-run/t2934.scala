import scala.util.continuations._

object Test {
  def main(args : Array[String]) {
   println(reset {
     val x = shift(List(1,2,3).flatMap[Int, List[Int]])
     List(x + 2)
   })
  }
}
