object Test {
  def main(args: Array[String]): Unit = Console.withErr(Console.out) { 
    try {
      xml.parsing.ConstructingParser.fromSource(io.Source.fromString("<foo>"), false).document()
    } catch {
      case e:Exception => println(e.getMessage)
    }
  } 

} 

