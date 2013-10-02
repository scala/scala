object Foo {
  def foreach( f : ((Int,Int)) => Unit ) {
    println("foreach")
    f(1,2)
  }

  for( (a,b) <- this ) {
    println((a,b))
  }
}
