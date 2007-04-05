class Quantity {
    def getValue = 0;
    def connect(c: Constraint) = c.newValue;
}

abstract class Constraint(q: Quantity) {
    def newValue: Unit;
    q connect this
}

class Adder(q: Quantity) extends Constraint(q) {
    def newValue = Console.println(q.getValue);
}

object Main {
    def main(args: Array[String]): Unit = {
        val x = new Quantity;
        new Adder(x);
        ()
    }
}
