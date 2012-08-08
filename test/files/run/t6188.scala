// SI-6188 Optimizer incorrectly removes method invocations containing throw expressions

import scala.util.Success

object Test {
 def main(args: Array[String]) {
   val e = new Exception("this is an exception")
   val res = Success(1).flatMap[Int](x => throw e)
   println(res)
 }
}

