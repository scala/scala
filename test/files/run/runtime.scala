//############################################################################
// Run Time Bugs & Test Cases
//############################################################################

//############################################################################
// Test 0 - Array creation

object Test0Test {
  def println[A](xs: Array[A]): Unit = {
    var i = 0;
    Console.print("[");
    while (i < xs.length) {
      if (i > 0) Console.print(",");
      Console.print(xs(i));
      i = i + 1;
    }
    Console.print("]");
    Console.println;
  }

  def test(args: Array[String]): Unit = {
    val zs: Array[Boolean] = Array(false, true);
    val bs: Array[Byte   ] = Array(0, 1, 2);
    val ss: Array[Short  ] = Array(3, 4, 5);
    val cs: Array[Char   ] = Array('a', 'b', 'c');
    val is: Array[Int    ] = Array(6, 7, 8);
    val ls: Array[Long   ] = Array(9l, 10l, 11l);
    val fs: Array[Float  ] = Array(12.0f, 13.0f);
    val ds: Array[Double ] = Array(14.0d, 15.0d);
    val os: Array[AnyRef ] = Array("string");
    println(zs);
    println(bs);
    println(ss);
    println(cs);
    println(is);
    println(ls);
    println(fs);
    println(ds);
    println(os);
  }
}

//############################################################################
// Test 1 - Block Qualifiers

package test1.bar {

  object System {
    val out: PrintStream = new PrintStream();
  }

  class PrintStream() {
    def println(): Unit = {
      Console.println;
    }
  }

}

object Test1Test {

  def test(args: Array[String]): Unit = {
    {Console.print(10)}; Console.println;
    // {System.out.print(11); java}.lang.System.out.println();
    // {System.out.print(12); java.lang}.System.out.println();
    // {System.out.print(13); java.lang.System}.out.println();
    {Console.print(14); Console}.println;
    {Console.print(15); (() => Console.println):(() => Unit)} apply ();
    {Console.print(16); Console.println};

    {Console.print(20)}; test1.bar.System.out.println();
    // {System.out.print(21); test1}.bar.System.out.println();
    // {System.out.print(22); test1.bar}.System.out.println();
    {Console.print(23); test1.bar.System}.out.println();
    {Console.print(24); test1.bar.System.out}.println();
    {Console.print(25); test1.bar.System.out.println _ : (() => Unit)} apply ();
    {Console.print(26); test1.bar.System.out.println()};
  }

}

//############################################################################
// Test 2 - Super Calls with Mixins

package test2 {

  class A {
    def run = Console.println("A");
  }

  trait M0 extends A {
    override def run = { super.run; Console.println("M0"); }
  }

  class M1 extends M0 {
    override def run = { super.run; Console.println("M1"); }
  }

  trait N0 extends A {
    override def run = { super.run; Console.println("N0"); }
  }

  class N1 extends N0 {
    override def run = { super.run; Console.println("N1"); }
  }

  object M0N0 extends M0 with N0;
  object N0M0 extends N0 with M0;
  object M1N0 extends M1 with N0;
  object N1M0 extends N1 with M0;

}

object Test2Test {
  def test(args: Array[String]): Unit = {
    test2.M0N0.run; Console.println;
    test2.N0M0.run; Console.println;
    test2.M1N0.run; Console.println;
    test2.N1M0.run; Console.println;
  }
}

//############################################################################
// Test 3 - Methods eq and ne

object Test3Test {

  class Foo { override def equals(that: Any) = sys.error("abort"); }

  def check(expected: Boolean, actual1: Boolean, actual2: Boolean): Unit =
    Console.println(
      if ((actual1 == expected) && (actual2 == !expected)) "Ok" else "KO: "
        + "expected: " + expected + " - " + (!expected) + ", "
        + "found: " + actual1 + " - " + actual1);

  def test(args: Array[String]): Unit = {
    val foo1: AnyRef = null;
    val foo2: AnyRef = new Foo();
    val foo3: AnyRef = new Foo();

    check(true , null eq null, null ne null);
    check(true , null eq foo1, null ne foo1);
    check(false, null eq foo2, null ne foo2);
    check(false, null eq foo3, null ne foo3);

    check(true , foo1 eq null, foo1 ne null);
    check(true , foo1 eq foo1, foo1 ne foo1);
    check(false, foo1 eq foo2, foo1 ne foo2);
    check(false, foo1 eq foo3, foo1 ne foo3);

    check(false, foo2 eq null, foo2 ne null);
    check(false, foo2 eq foo1, foo2 ne foo1);
    check(true , foo2 eq foo2, foo2 ne foo2);
    check(false, foo2 eq foo3, foo2 ne foo3);

    check(false, foo3 eq null, foo3 ne null);
    check(false, foo3 eq foo1, foo3 ne foo1);
    check(false, foo3 eq foo2, foo3 ne foo2);
    check(true , foo3 eq foo3, foo3 ne foo3);
  }

}

//############################################################################
// Main

object Test  {
  var errors: Int = 0;
  def test(name: String, test: => Unit): Unit = {
    Console.println("<<< " + name);
    try {
      test;
    } catch {
      case exception: Throwable => {
        //val name: String = Thread.currentThread().getName();
        Console.print("Exception in thread \"" + name + "\" " + exception);
        Console.println;
        errors = errors + 1;
      }
    }
    Console.println(">>> " + name);
    Console.println;
  }

  def main(args: Array[String]): Unit = {

    test("Test0"  , Test0Test.test(args));
    test("Test1"  , Test1Test.test(args));
    test("Test2"  , Test2Test.test(args));
    test("Test3"  , Test3Test.test(args));

    if (errors > 0) {
      Console.println;
      Console.println(errors + " error" + (if (errors > 1) "s" else ""));
    }
  }
}

//############################################################################
