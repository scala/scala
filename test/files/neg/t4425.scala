object Foo {
  object X { def unapply(x : Int)(y : Option[Int] = None) = None }
  42 match { case _ X _ => () }
}

object Foo2 {
  object X { def unapply(x : Int)(y: Int) = Some((2,2)) }
  42 match { case _ X _ => () }
}

object Foo3 {
  object X { def unapply(x : String)(y: String) = Some((2,2)) }
  "" match { case _ X _ => () }
}