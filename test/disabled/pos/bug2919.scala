import javax.xml.bind.annotation.adapters.XmlAdapter
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter

case class Link(
  @XmlJavaTypeAdapter(classOf[StringOptionAdapter]) val title: Option[String]
)

class StringOptionAdapter extends XmlAdapter[String, Option[String]] {
  def unmarshal(str: String) = error("stub")
  def marshal(op: Option[String]) = error("Stub")
}

