object Test { 
  import scala.xml.{Node,HasKeyValue}
  
  def domatch(x:Node): Node = {
    val hasBar = new HasKeyValue("bar")
    
    x match {
      case Node("foo", hasBar(z), _*) => z
      case _ => null
    }
  }
  
  def main(args: Array[String]): Unit = {
    println(domatch(<foo bar="baz"><hi/></foo>))
    println(domatch(<foo bingo="donkey"><hi/></foo>))
    // 
    // assert(domatch(<foo bar="baz"><hi/></foo>).toString == "baz")
    // assert(domatch(<foo bar="baz2"><hi/></foo>) == null)//, domatch(<foo bar="baz2"><hi/></foo>))
  }
}
