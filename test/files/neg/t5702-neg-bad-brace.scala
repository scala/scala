
object Test {

  def main(args: Array[String]) {
    val is = List(1,2,3)

    is match {
// the erroneous brace is ignored, so we can't halt on it.
// maybe brace healing can detect overlapping unmatched (...}
// In this case, the fix emits an extra error:
// t5702-neg-bad-brace.scala:10: error: Unmatched closing brace '}' ignored here
// t5702-neg-bad-brace.scala:10: error: illegal start of simple pattern (i.e., =>)
// t5702-neg-bad-brace.scala:11: error: ')' expected but '}' found.
      case List(1, _*} =>
    }
  }
}
