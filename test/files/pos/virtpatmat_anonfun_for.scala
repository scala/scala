trait Foo {
  def bla = {
    val tvs = "tvs"
    Nil.foreach(x => x match {
      case _ => println(tvs)
    })
  }
}