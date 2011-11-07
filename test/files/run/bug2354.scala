import scala.xml.parsing._
import scala.io.Source

object Test
{
  val xml_good = "<title><![CDATA[Hello [tag]]]></title>"
  val xml_bad = "<title><![CDATA[Hello [tag] ]]></title>"

  val parser1 = ConstructingParser.fromSource(Source.fromString(xml_good),false)
  val parser2 = ConstructingParser.fromSource(Source.fromString(xml_bad),false)
  
  def main(args: Array[String]): Unit = {
    parser1.document
    parser2.document
  }
}

