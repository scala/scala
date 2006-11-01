package test;

object main {

  class a {

    object b {

      trait c {}
      def foo(x: c): c = { Console.println("foo(" + x + ")"); x }

    }

    def bar(x: b.c): a.this.b.c = { b.foo(x); x }
  }

  def main(args: Array[String]) = {
    val aa = new a;
    val xx: aa.b.c = null;
    Console.println(aa.bar(xx));
  }
}
