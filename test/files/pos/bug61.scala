object O {

  class testClass ;

  case class testA() extends testClass ; // works if you leave away "extends..."
                                         // or if you write TestA
  def ga( x:testClass ) = x match {
	  case testA() => ()
  }
}
