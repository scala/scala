object Test extends Application {
  object m {
    val f = { x: Unit => () }
    Console.println("OK")
  }
  m;
}
