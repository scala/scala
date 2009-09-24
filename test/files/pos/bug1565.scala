object Bug1565 {
  object X0 { 0;  (a : Int, b : Int, c : Int) => println(List(a, b))   }
  def x() = { 0; (a : Int, b : Int) => println(List(a, b)) ; 0  }

  (a : Int, b : Int) => println(List(a, b))

  // various function syntaxes to exercise the parser
  val xs = List(1,2,3)
  xs.filter(x => x < 2)
  xs.filter((x) => x < 2)
  xs.filter { x => x < 2 }
  xs.filter { _ < 2 }
  xs.filter (_ < 2)
  xs.foreach { e =>
    val buf0 = e + 1
    buf0
  }
}
