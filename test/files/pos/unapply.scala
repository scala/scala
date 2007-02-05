object Test {
  val xs = List(1)
  val f: int = {
    xs match {
      case List(x) => x
    }
  }
}
