import xml.Node

object Test {
  def main(args: Array[String]): Unit = {
    val noAttr = <t/>
    val attrNull = <t a={ null: String }/>
    val attrNone = <t a={ None: Option[Seq[Node]] }/>
    assert(noAttr == attrNull)
    assert(noAttr == attrNone)

    println(noAttr)
    println(attrNull)
    println(attrNone)

    // Check if attribute order is retained
    println(<t a="1" d="2"/>)
    println(<t b="1" d="2"/>)
    println(<t a="1" b="2" c="3"/>)
    println(<t g="1" e="2" p:a="3" f:e="4" mgruhu:ji="5"/>)
  }
}
