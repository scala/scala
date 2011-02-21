object Test extends App{
  Stream.from(1) match { case Stream(1, 2, x @_*) => println("It worked!") }
}
