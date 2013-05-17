object Test {
  val arr = Array[Char]('a' to 'i': _*)
  var xs: CharSequence = arr
  val hash = xs.hashCode

  def check(chars: CharSequence) {
    println("\n[check '" + chars + "'] len = " + chars.length)
    chars match {
      case x: Predef.ArrayCharSequence  => assert(x.__arrayOfChars eq arr, ((x.__arrayOfChars, arr)))
      case x: runtime.ArrayCharSequence => assert(x.xs eq arr, ((x.xs, arr)))
      case x                            => assert(false, x)
    }

    0 until chars.length foreach { i =>
      println("sub(%s, %s) == '%s'".format(i, chars.length, chars.subSequence(i, chars.length)))
      println("sub(%s, %s) == '%s'".format(0, i, chars.subSequence(0, i)))
    }
    if (chars.length >= 2)
      check(chars.subSequence(1, chars.length - 1))
  }

  def main(args: Array[String]): Unit = {
    while (xs.length > 0) {
      check(xs)
      xs = xs.subSequence(0, xs.length - 1)
    }
  }
}
