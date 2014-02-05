object Test extends App {
  val s = "Scala"
  val d = 8
  val b = false
  val f = 3.14159
  val c = 'c'
  val t = new java.util.Date
  val x = new java.util.Formattable {
    def formatTo(ff: java.util.Formatter, g: Int, w: Int, p: Int): Unit = ff format "xxx"
  }

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

  // 3) flag mismatches
  f"$s%+ 0,(s"
  f"$c%#+ 0,(c"
  f"$d%#d"
  f"$d%,x"
  f"$d%+ (x"
  f"$f%,(a"
  f"$t%#+ 0,(tT"

  // 4) bad precisions
  f"$c%.2c"
  f"$d%.2d"
  f"%.2%"
  f"%.2n"
  f"$f%.2a"
  f"$t%.2tT"

  // 5) bad indexes
  f"%<s"
  f"%<c"
  f"%<tT"
  f"${8}%d ${9}%d%3$$d"
  f"${8}%d ${9}%d%0$$d"

  // warnings
  f"${8}%d ${9}%1$$d"
  f"$s%s $s%s %1$$<s"
  f"$s%s $s%1$$s"

  // 6) bad arg types
  f"$s%#s"

  // 7) misunderstood conversions
  f"$t%tG"
  f"$t%t"
  f"$s%10.5"

  // 8) other brain failures
  f"${d}random-leading-junk%d"
}
