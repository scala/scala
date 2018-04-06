object Test extends App{
  LazyList.from(1) match { case LazyList(1, 2, x @_*) => println("It worked!") }
}
