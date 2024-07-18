
//> using options -Wunused:imports -Werror

object X {
  val v = 27
}
object Y {
  val v = 42
}
object Main {
  import X._
  import Y.v
  def main(args: Array[String]) = println {
    //import Y.v // warns
    v
  }
}

