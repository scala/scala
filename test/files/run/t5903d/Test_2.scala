object Test extends App {
  import Interpolation._
  42 match {
    case t"$x" => println(x)
  }
}
