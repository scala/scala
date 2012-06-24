object Test {
	def f(a: Int*) = a match {
	  case 0 :: Nil => "List(0)! My favorite Seq!"
	  case _ => a.toString
	}
}
