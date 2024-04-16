//> using options -Vreflective-calls
object Test extends App {
  import Interpolation._
  42 match {
    case t"$x" => println(x)
    case x     => throw new MatchError(x)
  }
}
