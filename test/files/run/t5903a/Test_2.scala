object Test extends App {
  import NewQuasiquotes._
  SomeTree match {
    case nq"$x + $y" => println((x, y))
  }
}
