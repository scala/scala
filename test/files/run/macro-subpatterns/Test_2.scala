object Test extends App {
  42 match {
    case Extractor(a @ Extractor(b @ Extractor(c))) => println(a); println(b); println(c)
  }
}
