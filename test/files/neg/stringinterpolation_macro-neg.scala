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
  f"$d%,,d"

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
  f"$f%f %<d"
  f"%1$$d $f%f"
  f"${null}%s %<#s"
  // add tests from https://github.com/scala/scala/pull/4316/files

  // 7) misunderstood conversions
  f"$t%tG"
  f"$t%t"
  f"$s%10.5"

  // 8) other brain failures
  f"${d}random-leading-junk%d"

  // 9) malformed specifiers
  f"$d%1$$"
  f"$d%2147483648d"
  f"$d%99999999999999999999d"
  f"$f%.2147483648f"
  f"$d%2147483648$$d"
  f"$d\u"
  f"$d\u00"
  f"a\u00zz"
  f"$d%d%2147483648$$n"
  f"$d%d%2147483648$$<d"
  f"$d%d%0$$n"
  f"$d%d%0$$<d"
  f"$d%.2147483648d"

  // 10) flags the formatter rejects
  f"$d%-d"
  f"$d%0d"
  f"$d%-0 5d"
  f"$d%+ d"
  f"$f%-015f"
  f"$f%0f"
  f"$s%-s"
  f"$c%-c"
  f"$t%-tT"
  f"$d%d%1$$-d"
  f"$d%d%-%"
  f"$f%,e"
  f"$f%,E"
  f"$f%#g"
  f"$f%#G"

  // 11) an arg the formatter rejects
  f"${BigDecimal(1)}%a"
}
