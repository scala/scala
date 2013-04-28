import java.io.{ Console => _, _ }
import scala.io._
import scala.xml.parsing._
object Test {
  def main(args: Array[String]): Unit = {
    val xml = "<!DOCTYPE xmeml SYSTEM> <xmeml> <sequence> </sequence> </xmeml> "
    val sink = new PrintStream(new ByteArrayOutputStream())
    (Console withOut sink) {
      (Console withErr sink) {
        ConstructingParser.fromSource((Source fromString xml), true).document.docElem 
      }
    }
  }
}
