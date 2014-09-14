class TestSealedExhaustive { // compile only
    sealed abstract class Foo

    case class Bar(x:Int) extends Foo
    case object Baz extends Foo

    def ma1(x:Foo) = x match {
      case Bar(_) => // not exhaustive
    }

    def ma2(x:Foo) = x match {
      case Baz    => // not exhaustive
    }

    sealed abstract class Mult
    case class Kult(s:Mult) extends Mult
    case class Qult() extends Mult

    def ma33(x:Kult) = x match { // exhaustive
      case Kult(_) => // exhaustive
    }

    def ma3(x:Mult) = (x,x) match { // not exhaustive
      case (Kult(_), Qult())    => // Kult missing
      //case (Kult(_), Kult(_))    =>
      case (Qult(), Kult(_))    => // Qult missing
      //case (Qult(), Qult())    =>
    }

    def ma3u(x:Mult) = ((x,x) : @unchecked) match { // not exhaustive, but not checked!
      case (Kult(_), Qult())    =>
      case (Qult(), Kult(_))    =>
    }

    sealed abstract class Deep

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

    def ma4(x:Deep) = x match { // missing cases: Gu, Gp which is not abstract so must be included
      case Ga =>
    }

    def ma5(x:Deep) = x match {
      case Gu =>
      case _ if 1 == 0 =>
      case Ga =>
    }

  def ma6()  = List(1,2) match { // give up
    case List(1,2) =>
    case x :: xs =>
  }

  def ma7() = List(1,2) match { //exhaustive
    case 1::2::Nil =>
      case _ =>
  }

  sealed class B
  case class B1() extends B
  case object B2 extends B
  def ma8(x: B) = x match {
    case _: B => true
  }
  def ma9(x: B) = x match {
    case B1() => true       // missing B, which is not abstract so must be included
    case B2   => true
  }

  object ob1 {
    sealed abstract class C
    sealed abstract class C1 extends C
    object C2 extends C
    case class C3() extends C
    case object C4 extends C

    def ma10(x: C) = x match {  // exhaustive: abstract sealed C1 is dead end.
      case C3()     => true
      case C2 | C4  => true
    }
  }

  object ob2 {
    sealed abstract class C
    abstract class C1 extends C
    object C2 extends C
    case class C3() extends C
    case object C4 extends C

    def ma10(x: C) = x match { // not exhaustive: C1 is not sealed.
      case C3()     => true
      case C2 | C4  => true
    }
  }
  object ob3 {
    sealed abstract class C
    sealed abstract class C1 extends C
    object D1 extends C1
    case class D2() extends C1
    object C2 extends C
    case class C3() extends C
    case object C4 extends C

    def ma10(x: C) = x match {  // not exhaustive: C1 has subclasses.
      case C3()     => true
      case C2 | C4  => true
    }
  }
  object ob4 {
    sealed abstract class C
    sealed class C1 extends C
    object C2 extends C
    case class C3() extends C
    case object C4 extends C

    def ma10(x: C) = x match { // not exhaustive: C1 is not abstract.
      case C3()     => true
      case C2 | C4  => true
    }
  }
}
