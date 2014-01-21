package pkg { 
  package object other
  package other {
    class Crash { 
      AnyOps(0)
      ()
    }
  }
}
 
object Test {
  def main(args: Array[String]): Unit = {
    new pkg.other.Crash
  }
}
