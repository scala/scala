object Test {
import scala.io.Source.fromString
import scala.xml.parsing.ConstructingParser.fromSource
import scala.xml.TopScope
  def parse(s:String) = fromSource(fromString(s), false).element(TopScope)
	def main(argv : Array[String]) : Unit = {

                println(parse("<foo x='&amp;'/>"))
		println(xml.XML.loadString("<foo x='&amp;'/>"))
		println(<foo x="&amp;"/>)
		println(<foo x={ "&" }/>)

		println(xml.XML.loadString("<foo x='&amp;amp;'/>"))
                println(parse("<foo x='&amp;amp;'/>"))
		println(<foo x="&amp;amp;"/>)
		println(<foo x={ "&amp;" }/>)
		println(xml.XML.loadString("<foo x='&amp;&amp;'/>"))
                println(parse("<foo x='&amp;&amp;'/>"))
		println(<foo x="&amp;&amp;"/>)
		println(<foo x={ "&&" }/>)
	}
}
