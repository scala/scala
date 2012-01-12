import xml.Node

object Test {
  def main(args: Array[String]): Unit = {
    val noAttr = <t/>
    val attrNull = <t a={ null: String }/>
    val attrNone = <t a={ None: Option[Seq[Node]] }/>
    val preAttrNull = <t p:a={ null: String }/>
    val preAttrNone = <t p:a={ None: Option[Seq[Node]] }/>
    assert(noAttr == attrNull)
    assert(noAttr == attrNone)
    assert(noAttr == preAttrNull)
    assert(noAttr == preAttrNone)

    val noAttrStr = "<t></t>"
    assert(noAttr.toString() == noAttrStr)
    assert(attrNull.toString() == noAttrStr)
    assert(attrNone.toString() == noAttrStr)
    assert(preAttrNull.toString() == noAttrStr)
    assert(preAttrNone.toString() == noAttrStr)

    val xml1 = <t b="1" d="2"/>
    val xml2 = <t a={ null: String } p:a={ null: String } b="1" c={ null: String } d="2"/>
    val xml3 = <t b="1" c={ null: String } d="2" a={ null: String } p:a={ null: String }/>
    assert(xml1 == xml2)
    assert(xml1 == xml3)

    val xml1Str = "<t d=\"2\" b=\"1\"></t>"
    assert(xml1.toString() == xml1Str)
    assert(xml2.toString() == xml1Str)
    assert(xml3.toString() == xml1Str)
  }
}
