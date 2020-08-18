// scalac: -Xfatal-warnings
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
  def fb(x: Exh) = x match { case Foo(x)       => x         case Bar(x) => x } // optimistically exhaustive
  // ^ under -Xstrict-patmat-analysis pessimistically approximates case Foo(x) as inexhaustive:
  //     test/files/neg/t5365e.scala:12: warning: match may not be exhaustive.
  //     It would fail on the following input: Foo(_)
  //       def fb(x: Exh) = x match { case Foo(x)       => x         case Bar(x) => x } // optimistically exhaustive
  //                        ^
  // ... and the counter-example needs work -.- ...
}
