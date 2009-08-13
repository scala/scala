object Test extends Application {
  val ducks = Array[AnyRef]("Huey", "Dewey", "Louie");
  ducks.elements.asInstanceOf[Iterator[String]]
}
