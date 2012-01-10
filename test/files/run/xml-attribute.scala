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

    println(noAttr)
    println(attrNull)
    println(attrNone)
    println(preAttrNull)
    println(preAttrNone)

    val xml1 = <t b="1" d="2"/>
    val xml2 = <t a={ null: String } p:a={ null: String } b="1" c={ null: String } d="2"/>
    val xml3 = <t b="1" c={ null: String } d="2" a={ null: String } p:a={ null: String }/>
    assert(xml1 == xml2)
    assert(xml1 == xml3)

    println(xml1)
    println(xml2)
    println(xml3)

    // Check if attribute order is retained
    println(<t a="1" d="2"/>)
    println(<t b="1" d="2"/>)
    println(<t a="1" b="2" c="3"/>)
    println(<t g="1" e="2" p:a="3" f:e="4" mgruhu:ji="5"/>)
  }
}
