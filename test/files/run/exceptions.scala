//############################################################################
// Exceptions
//############################################################################
// $Id$

//############################################################################

import java.lang.System; // to avoid name clash with .NET's library

abstract class IntMap[A] {
    def lookup(key: Int): A = match {
        case Empty() => error("KO")
        case _ => error("ok")
    }
}

case class Empty[A]() extends IntMap[A];

object exceptions {

    def check(what: String, actual: Any, expected: Any): Unit = {
        val success: Boolean = actual == expected;
        System.out.print(if (success) "ok" else "KO");
        var value: String = if (actual == null) "null" else actual.toString();
        if (value == "\u0000") value = "\\u0000";
        System.out.print(": " + what + " = " + value);
        if (!success) System.out.print(" != " + expected);
        System.out.println();
        System.out.flush();
    }

    def test: Unit = {
        val key = 2000;
        val map: IntMap[String] = new Empty[String];
        val value = try {
            map.lookup(key)
        } catch {
            case e => e.getMessage()
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
