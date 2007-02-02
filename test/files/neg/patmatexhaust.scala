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
    def ma3(x:Mult) = {x,x} match { // not exhaustive
      case {Kult(_), Qult()}    => // Kult missing
      //case {Kult(_), Kult(_)}    =>
      case {Qult(), Kult(_)}    => // Qult missing
      //case {Qult(), Qult()}    =>
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

    def ma4(x:Deep) = x match { // missing cases: Gu
      case Ga =>
    }

    def zma5(x:Deep) = x match { // exhaustive
      case Gu =>
      case _ if 1 == 0 =>
      case Ga =>
    }

  def redundant = 1 match { // include this otherwise script won't test this in files/neg
    case 1 =>
      case 1 =>
  }
}
