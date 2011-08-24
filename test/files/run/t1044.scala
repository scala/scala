object Test extends App {
  val ducks = Array[AnyRef]("Huey", "Dewey", "Louie");
  ducks.iterator.asInstanceOf[Iterator[String]]
}
