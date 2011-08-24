import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import com.sun.xml.internal.fastinfoset._
import com.sun.xml.internal.fastinfoset.sax._
import scala.xml.parsing.NoBindingFactoryAdapter
import scala.xml._

// Note - this is in pending because com.sun.xml.etc is not standard,
// and I don't have time to extract a smaller test.

object Test {
	def main(args: Array[String]) {
		val node = <test/>
		val bytes = new ByteArrayOutputStream
		val serializer = new SAXDocumentSerializer()

		serializer.setOutputStream(bytes)
		serializer.startDocument()
		serialize(node, serializer)
		serializer.endDocument()
		println(parse(new ByteArrayInputStream(bytes.toByteArray)))
	}
	def serialize(node: Node, serializer: SAXDocumentSerializer) {
		node match {
		case _ : ProcInstr | _ : Comment | _ : EntityRef =>
		case x : Atom[_] =>
			val chars = x.text.toCharArray
			serializer.characters(chars, 0, chars.length)
		case _ : Elem =>
			serializer.startElement("", node.label.toLowerCase, node.label.toLowerCase, attributes(node.attributes))
			for (m <- node.child) serialize(m, serializer)
			serializer.endElement("", node.label.toLowerCase, node.label.toLowerCase)
		}
	}
	def parse(str: ByteArrayInputStream) = {
		val parser = new SAXDocumentParser
		val fac = new NoBindingFactoryAdapter

		parser.setContentHandler(fac)
		try {
			parser.parse(str)
		} catch {
		case x: Exception =>
			x.printStackTrace
		}
		fac.rootElem
	}
	def attributes(d: MetaData) = {
		val attrs = new AttributesHolder

		if (d != null) {
			for (attr <- d) {
				val sb = new StringBuilder()
				Utility.sequenceToXML(attr.value, TopScope, sb, true)
				attrs.addAttribute(new QualifiedName("", "", attr.key.toLowerCase), sb.toString)
			}
		}
		attrs
	}
}
