//############################################################################
// Exceptions
//############################################################################

//############################################################################

abstract class IntMap[A] {
    def lookup(key: Int): A = this match {
        case Empty() => sys.error("KO")
        case _ => sys.error("ok")
    }
}

case class Empty[A]() extends IntMap[A];

object exceptions {

    def check(what: String, actual: Any, expected: Any): Unit = {
        val success: Boolean = actual == expected;
        Console.print(if (success) "ok" else "KO");
        var value: String = if (actual == null) "null" else actual.toString();
        if (value == "\u0000") value = "\\u0000";
        Console.print(": " + what + " = " + value);
        if (!success) Console.print(" != " + expected);
        Console.println;
        Console.flush;
    }

    def test: Unit = {
        val key = 2000;
        val map: IntMap[String] = new Empty[String];
        val value = try {
            map.lookup(key)
        } catch {
            case e: Throwable => e.getMessage()
        }
        check("lookup(" + key + ")", value, "KO");
    }

}

//############################################################################

object Test {

  def main(args: Array[String]): Unit = {
    exceptions.test;
  }

}

//############################################################################
