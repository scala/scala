// contribution bug #460

object Test extends Application {
  val x = Some(3) match {
    case Some(1 | 2) => 1
    case Some(3) => 2
  }
  println(x)
}
