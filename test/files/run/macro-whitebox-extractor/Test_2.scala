object Test extends App {
  42 match {
    case Extractor(x) => println(x)
    case x => throw new MatchError(x)
  }
}
