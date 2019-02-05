
class C {
  val Some(`_`) = Option(42)  // was crashola

  def f(): Unit = {
    val Some(`_`) = Option(42)  // was crashola
  }
}
