
object Test {
  def main(args: Array[String]) {

    val xml = <top><a>apple</a><b>boy</b><c>child</c></top>
    // This is the more likely typo, and the uglier parse.
    // We could turn it into a } if } does not follow (to
    // avoid handing }} back to xml) but that is quite ad hoc.
    // Assuming } for ) after _* would not be not outlandish.
    // bad brace or paren after _*
    val <top>{a, z@_*)</top> = xml
    println("A for "+ a +", ending with "+ z)
  }
}
