//############################################################################
// Boolean Expressions
//############################################################################

class Counter {
  private var n: Int = 0;
  def incrThen(b: Boolean) = if (b) n += 1;
  def value = n;
}

object Test1 {
  var flag = false;
  def flip: Boolean = { val tmp = flag; flag = !flag; tmp }
  def run: Int = {
    val c = new Counter;
    c.incrThen(flip || flip);
    c.value
  }
}

object Test2 {
  val a = Array(false);

  def run: Int = {
    val c = new Counter;
    c.incrThen(true && a(0));
    c.incrThen(false || Nil.length > 0);
    c.value
  }
}

//############################################################################
// Test code

object Test {
  def check_success(name: String, closure: => Int, expected: Int) {
    Console.print("test " + name);
    try {
      val actual: Int = closure;
      if (actual == expected) {
        Console.print(" was successful");
      } else {
        Console.print(" failed: expected "+ expected +", found "+ actual);
      }
    } catch {
      case exception: Throwable =>
        Console.print(" raised exception " + exception);
    }
    Console.println;
  }

  def main(args: Array[String]) {
    check_success("Test1", Test1.run, 1);
    check_success("Test2", Test2.run, 0);
    Console.println;
  }
}

//############################################################################
