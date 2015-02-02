sealed abstract class Z
object Z {
   object     Z0    extends Z
   case class Z1()  extends Z
   object     Z2    extends Z
   case class Z3()  extends Z
   object     Z4    extends Z
   case class Z5()  extends Z
   object     Z6    extends Z
   case class Z7()  extends Z
   object     Z8    extends Z
   case class Z9()  extends Z
   object     Z10   extends Z
   case class Z11() extends Z
   object     Z12   extends Z
   case class Z13() extends Z
   object     Z14   extends Z
   case class Z15() extends Z
   object     Z16   extends Z
   case class Z17() extends Z
   object     Z18   extends Z
   case class Z19() extends Z
}

object Test {
  import Z._
  def foo(z: Z) = z match {
    case Z0  | Z1()  | Z2  | Z3()  | Z4  | Z5()  | Z6  | Z7()  | Z8  | Z9() |
         Z10 | Z12 | Z13() | Z14 | Z15() | Z16 | Z17() | Z18 | Z19()
         =>
  }
}
