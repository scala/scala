object Test {
  import xml.XML.loadString
  def mkElem(arg: String) = <foo a="1" b="2" c="3" d="4" e={arg} />
  
  val x1 = mkElem("5")
  val x2 = mkElem("50")

  def main(args: Array[String]): Unit = {    
    assert(x1 == loadString("" + x1))
    assert(x2 != loadString("" + x1))
  }
}
