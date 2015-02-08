
import scala.tools.partest.ParserTest


object Test extends ParserTest {

  override def code = """
  trait X {
    // error: in XML literal: name expected, but char '!' cannot start a name
    def x = <![CDATA[hi & bye]]> <![CDATA[red & black]]>
  }
  trait Y {
    def y = <a><b/>start<![CDATA[hi & bye]]><c/>world<d/>stuff<![CDATA[red & black]]></a>
  }
  """

  override def extraSettings = s"${super.extraSettings} -Xxml:coalescing"
}
