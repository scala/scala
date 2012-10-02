object Main {
  def main(args : Array[String]) {
    val fn = (a : Int, str : String) => "a: " + a + ", str: " + str
    implicit def fx[T](f : (T,String) => String) = (x:T) => f(x,null)
    println(fn(1))
    ()
  }
}
