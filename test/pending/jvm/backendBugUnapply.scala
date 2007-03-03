object Test { import scala.xml.{Node,HasKeyValue}
  def domatch(x:Node): Node = {
    val hasBar = new HasKeyValue("bar")
    x match {
      case Node("foo", hasBar(z), _*) => z
      case _ => null
    }
  }
}
