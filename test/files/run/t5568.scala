object Test {
  def main(args: Array[String]): Unit = {
    // these should give unboxed results
    println(().getClass)
    println(5.getClass)
  	// these should give boxed results
    println(().asInstanceOf[AnyRef with Unit].getClass)
    println(().asInstanceOf[Unit with AnyRef].getClass)
    println(5.asInstanceOf[AnyRef with Int].getClass)
    println(5.asInstanceOf[Int with AnyRef].getClass)
    //make sure ## wasn't broken
    println(5.##)
    println((5.asInstanceOf[AnyRef]).##)
    println((5:Any).##)
  }
}
