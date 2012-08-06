// SI-6188 Optimizer incorrectly removes method invocations containing throw expressions

case class Test[T](x: T) {
  def map[S](f: T => S): Test[S] = {
    println("enter Test.map")
    Test(f(x))
  }
}

object Test {
 def main(args: Array[String]) {
   val e = new Exception("this is an exception")
   try {
     val res = Test(1).map[Int](x => throw e)
     println(res)
   } catch {
     case e: Exception =>
       println("OK")
   }
 }
}

