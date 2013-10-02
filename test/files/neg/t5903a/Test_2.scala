object Test extends App {
  import NewQuasiquotes._
  SomeTree match {
    case nq"$x + $y + $z" => println((x, y))
  }
}
