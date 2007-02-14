class TestSealedExhaustive { // compile only

    sealed class Foo

    case class Bar(x:Int) extends Foo
    case object Baz extends Foo

    def ma1(x:Foo) = x match {
      case Bar(_) => // not exhaustive
    }

    def ma2(x:Foo) = x match {
      case Baz    => // not exhaustive
    }

    sealed class Mult
    case class Kult(s:Mult) extends Mult
    case class Qult() extends Mult

    def ma33(x:Kult) = x match { // exhaustive
      case Kult(_) => // exhaustive
    }
    def ma3(x:Mult) = (x,x) match { // not exhaustive
      case (Kult(_), Qult())    => // Kult missing
      //case Pair(Kult(_), Kult(_))    =>
      case (Qult(), Kult(_))    => // Qult missing
      //case Pair(Qult(), Qult())    =>
    }

    sealed class Deep

    case object Ga extends Deep
    sealed class Gp extends Deep
    case object Gu extends Gp

    def zma3(x:Deep) = x match { // exhaustive!
      case _ =>
    }
    def zma4(x:Deep) = x match { // exhaustive!
      case Ga =>
      case _ =>
    }

    def ma4(x:Deep) = x match { // missing cases: Gu, Gp
      case Ga =>
    }

    def ma5(x:Deep) = x match { // Gp
      case Gu =>
      case _ if 1 == 0 =>
      case Ga =>
    }

  def ma6  = List(1,2) match { // give up
    case List(1,2) =>
    case x :: xs =>
  }

  def ma7 = List(1,2) match { //exhaustive
    case 1::2::Nil =>
      case _ =>
  }
  def redundant = 1 match { // include this otherwise script won't test this in files/neg
    case 1 =>
      case 1 =>
  }
}
