//############################################################################
// Overloads
//############################################################################
// $Id$

//############################################################################

object M1 {
    def f[A](x: A) = 11;
    def f[A <: StructuralEquality[A]](x: A) = 12;
}

object M2 {
    def f[A <: StructuralEquality[A]](x: A) = 21;
    def f[A](x: A) = 22;
}

object overloads {

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
        val x = 3;
        check("M1.f(" + x +")", M1.f(x), 11);
        check("M2.f(" + x +")", M2.f(x), 22);
        val y = new scala.collection.mutable.Stack[Int];
        check("M1.f(" + y +")", M1.f(y), 12);
        check("M2.f(" + y +")", M2.f(y), 21);
    }

}

//############################################################################

object Test {

  def main(args: Array[String]): Unit = {
    overloads.test;
  }

}

//############################################################################
