object Test {
  type Id[X] = X
  val a: Id[Option[Int]] = None

  a match {
     case Some(x) => println(x)
     case None    =>
  }
}
