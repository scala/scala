object Test extends App {
  val s = "Scala"
  val d = 8
  val b = false
  val f = 3.14159

  // 1) number of arguments
  new StringContext().f()
  new StringContext("", " is ", "%2d years old").f(s)
  new StringContext("", " is ", "%2d years old").f(s, d, d)
  new StringContext("", "").f()

  // 2) Interpolation mismatches
  f"$s%b"
  f"$s%c"
  f"$f%c"
  f"$s%x"
  f"$b%d"
  f"$s%d"
  f"$f%o"
  f"$s%e"
  f"$b%f"

  {
    implicit val strToInt1 = (s: String) => 1
    implicit val strToInt2 = (s: String) => 2
    f"$s%d"
  }

  f"$s%i"
}
