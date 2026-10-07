
object Test {
  import scala.xml._

  def main(args: Array[String]): Unit = {
    val xml =  <hello>world</hello>
    assert(xml.toString == "helloworld")
    val nodeSeq: NodeBuffer = <hello/><world/>
    assert(nodeSeq.mkString == "helloworld")
    val subSeq: scala.xml.Elem = <a><b/><c/></a>
    assert(subSeq.child.mkString == "bc")
    assert(subSeq.child.toString == "Vector(b, c)") // implementation detail

    val attrSeq: Elem = <a foo="txt&entityref;txt"/>
    assert(attrSeq.attributes.asInstanceOf[UnprefixedAttribute].value.toString == "Vector(txt, &entityref;, txt)")

    val g: Group = <xml:group><a/><b/><c/></xml:group>
    assert(g.nodes.toString == "Vector(a, b, c)")
  }
}
