object Test1 {
  object A { implicit val x: Int = 1 }
  import A.x
  def x: Int = 0
  implicitly[Int]
}
