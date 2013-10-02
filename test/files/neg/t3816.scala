class B {
    def ::(a: List[Int]) {
        a match {
            case x::xs =>
            case _ =>
        }
    }
}

object Test {
  def testSuccess1( x: Any ) = {
    val stable = 2
    x match {
      case Some( `stable` ) =>
      case _ =>
    }
  }

  val bar = 3
  def testSuccess2( x: Any ) = {
    x match {
      case Some( `bar` ) =>
      case _ =>
    }
  }

  def testFail1( x: Any ) = {
    var syncID = 0
    x match {
      case Some( `syncID` ) =>
      case _ =>
    }
  }

  var foo = 0
  def testFail2( x: Any ) = {
    x match {
      case Some( `foo` ) =>
      case _ =>
    }
  }
}
