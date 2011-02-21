object Test extends App {
  val src = scala.io.Source.fromString("<?xml version='1.0' encoding='UTF-8'?><feed/>")
  val parser = xml.parsing.ConstructingParser.fromSource(src, true)
  println(parser.document)
}

