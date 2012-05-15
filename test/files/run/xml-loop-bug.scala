object Test {
  def main(args: Array[String]): Unit = {
    val sink = new java.io.PrintStream(new java.io.ByteArrayOutputStream())
    Console setOut sink
    Console setErr sink
    scala.xml.parsing.ConstructingParser.fromSource(scala.io.Source.fromString("<!DOCTYPE xmeml SYSTEM> <xmeml> <sequence> </sequence> </xmeml> "), true).document.docElem 
  }
}
