// looks OK
object Main extends Application{
 val ducks = Array[AnyRef]("Huey", "Dewey", "Louie")
 ducks.iterator/*.asInstanceOf[Iterator[String]]*/
}
