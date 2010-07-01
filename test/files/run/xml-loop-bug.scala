object Test {
  def main(args: Array[String]): Unit = {
    scala.xml.parsing.ConstructingParser.fromSource(scala.io.Source.fromString("<!DOCTYPE xmeml SYSTEM> <xmeml> <sequence> </sequence> </xmeml> "), true).document.docElem
  }
}
