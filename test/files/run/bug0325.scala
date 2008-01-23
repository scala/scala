case class RS(self: String) {
  def split(separator: Char): Array[String] = self.split("\\Q"+separator+"\\E")

  def split(separators: Array[Char]): Array[String] = {
    val re = separators.foldLeft("[")(_+"\\Q"+_+"\\E") + "]"
    self.split(re)
  }
}

object Test {
  def expect = List("a","b")
  def test(f: => Array[String], which: String) {
    try {
      val ret = f.toList
      if (ret != expect)
        println(which + " returned " + ret + " when expecting " + expect)
      else
        println(ret)
    } catch {
      case e@_ => println(which + " failed with " + e.getClass)
    }
  }

  def main(args: Array[String]) {
    val badChars = "?*{+([\\^.$"

    for (c <- badChars)
      test(("a"+c+"b").split(c),"RichString split('"+ c + "')")
    println

    for (c <- badChars)
      test(RS("a"+c+"b").split(c),"RS split('"+ c + "')")
    println

    val badCases = List(
      ']' -> "x]", '&' -> "&&",'\\' -> "\\x", '[' -> "[x",
      '^' -> "^x", '-' -> "x-z"
    )
    for ((c,str) <- badCases)
      test(("a"+c+"b").split(str.toArray),"RichString split(\""+ str + "\")")
    println

    for ((c,str) <- badCases)
      test(RS("a"+c+"b").split(str.toArray),"RS split(\""+ str + "\")")
  }
}
