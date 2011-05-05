object Foo {
  object X { def unapply(x : Int)(y : Option[Int] = None) = None }
  42 match { case _ X _ => () }
}
