//> using options -Xfatal-warnings
sealed trait Exh
final case class Foo(xs: String*) extends Exh
final case class Bar(x: String)   extends Exh

class Main {
  def ex(x: Exh) = x match { case Foo(xs @ _*) => xs        case Bar(x) => x } // exhaustive
  def f0(x: Exh) = x match { case Foo()        => ()                         } // don't back off
  def f1(x: Exh) = x match { case Foo(x)       => x                          } // don't back off
  def f2(x: Exh) = x match { case Foo(x, y)    => x + y                      } // don't back off
  def fX(x: Exh) = x match { case Foo(xs @ _*) => xs                         } // don't back off
  def b1(x: Exh) = x match {                                case Bar(x) => x } // inexhaustive
  def fb(x: Exh) = x match { case Foo(x)       => x         case Bar(x) => x } // pessimistically inexhaustive
}
