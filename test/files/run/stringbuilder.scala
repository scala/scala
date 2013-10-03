
import scala.language.reflectiveCalls

object Test extends App {
  val str = "ABCDEFGHIJKLMABCDEFGHIJKLM"
  val surrogateStr = "an old Turkic letter: \uD803\uDC22"

  type SB = {
    def indexOf(str: String): Int
    def indexOf(str: String, fromIndex: Int): Int
    def lastIndexOf(str: String): Int
    def lastIndexOf(str: String, fromIndex: Int): Int
  }

  import scala.collection.mutable.{ StringBuilder => ScalaStringBuilder }
  import java.lang.{ StringBuilder => JavaStringBuilder }

  val sbScala = new ScalaStringBuilder() append str
  val sbJava = new JavaStringBuilder() append str
  val sbs: List[SB] = List[SB](sbScala, sbJava)

  def sameAnswers(f: (SB) => Int) = assert(f(sbScala) == f(sbJava))

  sameAnswers(_.indexOf(""))
  sameAnswers(_.indexOf("G"))
  sameAnswers(_.indexOf("ABC"))
  sameAnswers(_.indexOf("KLM"))
  sameAnswers(_.indexOf("QZV"))
  sameAnswers(_.indexOf("LMABC"))
  sameAnswers(_.lastIndexOf(""))
  sameAnswers(_.lastIndexOf("M"))
  sameAnswers(_.lastIndexOf("ABCDEFG"))
  sameAnswers(_.lastIndexOf("KLM"))
  sameAnswers(_.lastIndexOf("QZV"))
  sameAnswers(_.lastIndexOf("GHI", 22))
  sameAnswers(_.lastIndexOf("KLM", 22))

  // testing that the "reverse" implementation avoids reversing surrogate pairs
  val jsb = new JavaStringBuilder(surrogateStr).reverse
  val ssb = new ScalaStringBuilder(surrogateStr).reverseContents

  assert(jsb.toString == ssb.toString)
}
