object Test extends Application {
  val xs = Array(1, 2, 3)
  Console.println(xs.filter(_ >= 0).length)
}
