object Members {
  
  //unicode escapes don't get expanded in comments
  def comment = "comment"    // \u000A is the bomb
  
  //unicode escapes work in string
  def inString = "\u000A"
  def inTripleQuoted = """\u000A"""
  def inInterpolation = raw"\u000A"
  def inTripleQuotedInterpolation = raw"""\u000A"""
  def inChar = '\u000A'
  def `in backtick quoted\u0020identifier` = "bueno"

  def reasonableWaysToWriteTab = List(
    "literal tab in single quoted string" -> "tab	tab",
    "tab escape char in single quoted string" -> "tab\ttab",
    "tab unicode escape in single quoted string" -> "tab\u0009tab",
    "literal tab in triple quoted string" -> """tab	tab""",
    "literal tab in triple quoted raw interpolator" -> raw"""tab	tab""",
    "literal tab in single quoted raw interpolator" -> raw"tab	tab",
    "literal tab in triple quoted s interpolator" -> s"""tab	tab""",
    "literal tab in single quoted s interpolator" -> s"tab	tab",
    "tab escape char in triple quoted s interpolator" -> s"""tab\ttab""",
    "tab escape char in single quoted s interpolator" -> s"""tab\ttab""",
    "tab unicode escape in triple quoted s interpolator" -> s"""tab\u0009tab""",
    "tab unicode escape in single quoted s interpolator" -> s"""tab\u0009tab"""
  )

  def unreasonableWaysToWriteTab = List(
    "tab unicode escape in triple quoted string" -> """tab\u0009tab""",
    "tab unicode escape in raw interpolator" ->  raw"""tab\u0009tab"""
  )

  def waysNotToWriteTab = List(
    "tab escape char in triple quoted string" -> """tab\ttab""",
    "tab escape char in raw interpolator" -> raw"""tab\ttab"""
  )

}

object Test {
  val bueono = Members.`in backtick quoted identifier`
  
  def printways(ways: List[(String, String)]) = ways.map(_._1).sorted.mkString(", ")

  def main(args: Array[String]): Unit = {
   
    println("resonable ways to output a tab character")

    for {
      variation <- Members.reasonableWaysToWriteTab.groupBy(_._2).toList.map {
        case (written, ways) => s"written: $written - ways: ${printways(ways)}"
      }
    } println(variation)

    println("reasonable ways to output the character sequence \\t")
  
    for {
      variation <- Members.waysNotToWriteTab.groupBy(_._2).toList.map {
        case (written, ways) => s"written: $written - ways: ${printways(ways)}"
      }
    } println(variation)
  
  
    println("unreasonable ways to output a tab character")
  
    for {
      variation <- Members.unreasonableWaysToWriteTab.groupBy(_._2).toList.map {
        case (written, ways) => s"written: $written - ways: ${printways(ways)}"
      }
    } println(variation)
  }


}