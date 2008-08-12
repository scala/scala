object Test {
  List(1) match {
    case List(x) | List() => Console.println(x)
  }
  List(2) match {
    case List(_: Int) | List() => Console.println()
  }
}
