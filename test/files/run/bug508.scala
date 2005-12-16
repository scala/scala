object Test {
  case class Operator(x: Int);
  val EQ = new Operator(2);

  def main(args: Array[String]): Unit = {
    val x = Pair(EQ, 0);
    analyze(x); // should print "0"
    val y = Pair(EQ, 1);
    analyze(y); // should print "1"
    val z = Pair(EQ, 2);
    analyze(z); // should print "2"
  }

  def analyze(x: Pair[Operator, Int]) = x match {
    case Pair(EQ, 0) => Console.println("0")
    case Pair(EQ, 1) => Console.println("1")
    case Pair(EQ, 2) => Console.println("2")
    case _ => Console.println("undefined on " + x)
  }
}
