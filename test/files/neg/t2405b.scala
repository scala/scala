object A { implicit val x: Int = 1 }

// Expecting shadowing #1
object Test2 {
	{
		import A.{x => y}
		def y: Int = 0
		implicitly[Int]
	}
}
