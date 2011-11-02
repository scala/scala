object Test { 
  import scala.xml.{Node,UnprefixedAttribute}
  
  def domatch(x:Node) =
    x match {
      case Node("foo", UnprefixedAttribute("bar", z, _), _*) => z
      case _ => null
    }
  
  def main(args: Array[String]): Unit = {
    println(domatch(<foo bar="baz"><hi/></foo>))
    println(domatch(<foo bingo="donkey"><hi/></foo>))
    // 
    // assert(domatch(<foo bar="baz"><hi/></foo>).toString == "baz")
    // assert(domatch(<foo bar="baz2"><hi/></foo>) == null)//, domatch(<foo bar="baz2"><hi/></foo>))
  }
}
