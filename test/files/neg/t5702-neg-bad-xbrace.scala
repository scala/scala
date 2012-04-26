
object Test {
  def main(args: Array[String]) {
    /* PiS example, minus a brace
    val yearMade = 1965
    val old =
      <a>{ if (yearMade < 2000) <old>yearMade}</old>
           else xml.NodeSeq.Empty } </a>
    println(old)
    */

    // bad brace or paren after _*
    // actually, we know it's a bad paren...
    // we skip it because not in a context looking for rparen
    val xyear = <year>1965</year>
    val ancient =
      <b>{
        val when = xyear match {
          case <year>{_*)}</year> => y
          case _ => "2035"
        }
        <old>{when}</old>
      }</b>
    println(ancient)

    val xml = <top><a>apple</a><b>boy</b><c>child</c></top>
    // bad brace or paren after _*
    val <top>{a, z@_*)}</top> = xml
    println("A for "+ a +", ending with "+ z)
  }
}
