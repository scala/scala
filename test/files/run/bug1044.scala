object Test extends App {
  val ducks = Array[AnyRef]("Huey", "Dewey", "Louie");
  ducks.elements.asInstanceOf[Iterator[String]]
}
