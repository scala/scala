import scala.util.continuations._
object Test {

  def foo(x:Int) = {
    try { 
      throw new Exception
      shiftUnit0[Int,Int](7)
    } catch {
      case ex =>  
        val g = (a:Int)=>a
        9
    }
  }
  
  def main(args: Array[String]) {
    println(reset(foo(0)))
  }

}