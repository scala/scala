import xml.Node

object Test {
  def main(args: Array[String]): Unit = {
    val noAttr = <t/>
    val attrNull = <t a={ null: String }/>
    val attrNone = <t a={ None: Option[Seq[Node]] }/>
    assert(noAttr == attrNull)
    assert(noAttr == attrNone)
    assert(noAttr.toString() == "<t></t>")
    assert(attrNull.toString() == "<t></t>")
    assert(attrNone.toString() == "<t></t>")
  }
}
