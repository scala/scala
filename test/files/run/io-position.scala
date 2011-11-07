object Test {
  Console.setErr(Console.out)
  
  def main(args: Array[String]): Unit = { 
    try {
      xml.parsing.ConstructingParser.fromSource(io.Source.fromString("<foo>"), false).document()
    } catch {
      case e:Exception => println(e.getMessage)
    }
  } 

} 

