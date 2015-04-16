
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
  trait Z {
    def d = <![CDATA[hello, world]]>
    def e = <![CDATA[hello, world]]><![CDATA[hello, world]]>            // top level not coalesced
    def f = <foo>x<![CDATA[hello, world]]></foo>                        // adjoining text
    def g = <foo><![CDATA[hello, world]]></foo>                         // text node when coalescing
    def h = <foo><![CDATA[hello, world]]><![CDATA[hello, world]]></foo>
  }
  """

  // default coalescing behavior, whatever that is today.
  //override def extraSettings = s"${super.extraSettings} -Xxml:coalescing"
}
