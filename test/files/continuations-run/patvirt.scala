import scala.util.continuations._

object Test {
  def sh(x1:Int) = shift( (k: Int => Int) => k(k(k(x1))))
  
  def test(x1: Int) = {
    val o7 = {
      val o6 = {
        val o3 = 
          if (7 == x1) Some(x1)
          else None

        if (o3.isEmpty) None
        else Some(sh(x1))
      }
      if (o6.isEmpty) {
        val o5 =
          if (8 == x1) Some(x1)
          else None

        if (o5.isEmpty) None
        else Some(sh(x1))
      } else o6
    }
    o7.get
  }

  def main(args: Array[String]): Any = {
    println(reset(1 + test(7)))
    println(reset(1 + test(8)))
  }
}
