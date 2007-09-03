object A1 {
 object A2 {
   class X { def unapply(v : Int) = Some(v + 1) }
 }
}

object B1 {
  object B2 {
    val q = new A1.A2.X
  }
}

object Test {
  def main(args: Array[String]) {
    import B1.B2.q
    val res = 5 match { case q(x) => x }
    println(res)
  }
}



/*
compiler crash:

object A1 {
 object A2 {
   class X { def unapply(v : Int) = Some(v + 1) }
 }
}

object B1 {
  object B2 {
    val q = new A1.A2.X
  }
}

object C {
  def main(args: Array[String]) {
    //import B1.B2.q
    val q = new A1.A2.X
    val res = 5 match { case q(x) => x }
    println(res)
  }
}

*/
