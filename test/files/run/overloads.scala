//############################################################################
// Overloads
//############################################################################

//############################################################################

object Ops {
    def - = 0;
    def -(c: Char) = c;
    def -(i: Int) = i;

    def -- = 0;
    def --(c: Char) = c;
    def --(i: Int) = i;
}

object Funcs {
    def foo = 0;
//  def foo() = 1;
    def foo(c: Char) = 2;
    def foo(i: Int) = 3;
}

object M1 {
    def f[A](x: A) = 11;
    def f[A <: Ordered[A]](x: Ordered[A]) = 12;
}

object M2 {
    def f[A <: Ordered[A]](x: Ordered[A]) = 21;
    def f[A](x: A) = 22;
}

object overloads {

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

    def - = 0;
    def -(c: Char) = c;
    def -(i: Int) = i;

    def -- = 0;
    def --(c: Char) = c;
    def --(i: Int) = i;

    def test: Unit = {
        check("-('a')", -('a'), -97);
        check("-(97)", -(97), -97);

        check("Ops.-('a')", Ops.-('a'), 'a');
        check("Ops.-(97)", Ops.-(97), 97);

        check("--", --, 0);
        check("--('a')", --('a'), 'a');
        check("--(97)", --(97), 97);

        check("Ops.--", Ops.--, 0);
        check("Ops.--('a')", Ops.--('a'), 'a');
        check("Ops.--(97)", Ops.--(97), 97);

        check("Funcs.foo", Funcs.foo, 0);
//      check("Funcs.foo()", Funcs.foo(), 1);
        check("Funcs.foo('a')", Funcs.foo('a'), 2);
        check("Funcs.foo(97)", Funcs.foo(97), 3);

        val x = 3;
        check("M1.f(" + x +")", M1.f(x), 11);
        check("M2.f(" + x +")", M2.f(x), 22);
//       val y = new scala.collection.mutable.Stack[Int];
//      check("M1.f(" + y +")", M1.f(y), 12);
//      check("M2.f(" + y +")", M2.f(y), 21);
    }

}

//############################################################################

object Test {

  def main(args: Array[String]): Unit = {
    overloads.test;
  }

}

//############################################################################
