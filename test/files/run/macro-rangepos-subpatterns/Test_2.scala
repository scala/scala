//> using options -Yrangepos
object Test extends App {
  42 match {
    case Extractor(a) => println(a)
    case x => throw new MatchError(x)
  }
}
