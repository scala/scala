object Test {
  def analyze(x: Any) = x match {
    case s: String => println("It's a string: " + s)
    case 1 => println("It's a one")
    case (a: Int, b) => println("It's a pair of and int " + a +
                                " and something " + b)
    case 1 :: 2 :: _ => println("It's a list starting with 1, 2")
    case List(a, b, c) => println("It's a three-element list with " +
                                  a + ", " + b + ", " + c)
    case _ => println("It's something different")
  }
}
