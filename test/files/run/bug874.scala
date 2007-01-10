object Test {
  abstract class Base {
    type T;
    def T : T;
    def U[A](x1: A)(x2: int): Any
    T;
    U("xyz")(2)
  }
  class Mix extends Base {
    case class T {
      Console.println("T created")
    }
    case class U[A](x1: A)(x2: int) {
      Console.println("U created with "+x1+" and "+x2)
    }
  }
  def main(args : Array[String]) : Unit = {
    val obvious: Base = new Mix;
    obvious.T
    obvious.U("abc")(1)
  }
}
