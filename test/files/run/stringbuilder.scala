object Test extends App {
  val str = "ABCDEFGHIJKLMABCDEFGHIJKLM"
  val surrogateStr = "an old Turkic letter: \uD803\uDC22"

  import scala.collection.mutable.{ StringBuilder => ScalaStringBuilder }
  import java.lang.{ StringBuilder => JavaStringBuilder }

  val sbScala = new ScalaStringBuilder() append str
  val sbJava = new JavaStringBuilder() append str

  def sameAnswers(s: String, i: Int = -1, l: Boolean = false) = {
    if (l) {
      if (i == -1) assert(sbScala.lastIndexOf(s) == sbJava.lastIndexOf(s))
      else assert(sbScala.lastIndexOf(s, i) == sbJava.lastIndexOf(s, i))
    } else {
      if (i == -1) assert(sbScala.indexOf(s) == sbJava.indexOf(s), s"$s -- $sbScala -- $sbJava")
      else assert(sbScala.indexOf(s, i) == sbJava.indexOf(s, i))
    }
  }

  sameAnswers("")
  sameAnswers("G")
  sameAnswers("ABC")
  sameAnswers("KLM")
  sameAnswers("QZV")
  sameAnswers("LMABC")
  sameAnswers("", l = true)
  sameAnswers("M", l = true)
  sameAnswers("ABCDEFG", l = true)
  sameAnswers("KLM", l = true)
  sameAnswers("QZV", l = true)
  sameAnswers("GHI", 22, l = true)
  sameAnswers("KLM", 22, l = true)

  // testing that the "reverse" implementation avoids reversing surrogate pairs
  val jsb = new JavaStringBuilder(surrogateStr).reverse()
  val ssb = new ScalaStringBuilder(surrogateStr).reverseInPlace()

  assert(jsb.toString == ssb.toString)
}
