object Test extends App {
  object m {
    val f = { x: Unit => () }
    Console.println("OK")
  }
  m;
}
