object Test {
  List(1) match {
    case List(x) | List() => Console.println(x)
  }
  List(2) match {
    case List(_: int) | List() => Console.println()
  }
}
