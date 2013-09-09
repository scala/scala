object Test extends App {
  import Interpolation._
  2 match {
    case t"$x" => println(x)
  }
}
