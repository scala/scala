object Test
{
  val xml1 = <root xmlns:ns="nsUri" ns:at="rootVal"><sub ns:at="subVal"/></root>
  val xml2= scala.xml.XML.loadString("""<root xmlns:ns="nsUri" ns:at="rootVal"><sub ns:at="subVal"/></root>""")
  
  def backslashSearch(x: xml.Elem) = "root:-"+(x \ "@{nsUri}at") +"-sub:-"+(x \ "sub" \ "@{nsUri}at") +"-"
  
  def main(args: Array[String]): Unit = {
    println(backslashSearch(xml1))
    println(backslashSearch(xml2))
  }
}
