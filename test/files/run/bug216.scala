object Test extends Application {
  object m {
    val f = { x: unit => () }
    Console.println("OK")
  }
  m;
}
