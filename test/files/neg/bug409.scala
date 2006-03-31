abstract class Expr;
case class Case1(x: Int) extends Expr;
case class Case2(x: Int) extends Expr;
case class Case3(x: Int) extends Expr;

class Toto extends Expr with Case1(12);

object Main {
  def f(x: Expr): Int = x match {
    case Case1(x) => x
    case Case2(x) => x
    case Case3(x) => x
  }

  def main(args: Array[String]): Unit = {
    Console.println(f(new Toto));
  }
}
