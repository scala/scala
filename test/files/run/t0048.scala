object A1 {
 object A2 {
   class X { def unapply(v : Int) = Some(v + 1) }
 }
}

object Test {
  def main(args: Array[String]) {
    val q = new A1.A2.X
    val res = 5 match { case q(x) => x }
    println(res)
  }
}
