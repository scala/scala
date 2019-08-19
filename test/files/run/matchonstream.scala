object Test extends App{
  LazyList.from(1) match { case LazyList(1, 2, x @_*) => println(s"It worked! (class: ${x.getClass.getSimpleName})") }
}
