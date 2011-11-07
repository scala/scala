object Test {
  List(1) match {
    case List(x) | List() => Console.println(x)
  }  
}
