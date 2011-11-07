  final object Test extends java.lang.Object with Application {

    class Foo(val s: String, val n: Int) extends java.lang.Object {
    };

    def foo[A >: Nothing <: Any, B >: Nothing <: Any, C >: Nothing <: Any]
      (unapply1: (A) => Option[(B, C)], v: A): Unit = 
         unapply1.apply(v) match {
           case Some((fst @ _, snd @ _)) => 
              scala.Predef.println(scala.Tuple2.apply[java.lang.String, java.lang.String]("first: ".+(fst), " second: ".+(snd)))
           case _ => scala.Predef.println(":(")
    }           
    Test.this.foo[Test.Foo, String, Int]({
      ((eta$0$1: Test.Foo) => Test.this.Foo.unapply(eta$0$1))
    }, Test.this.Foo.apply("this might be fun", 10));
    final object Foo extends java.lang.Object with ((String, Int) => Test.Foo) {
      def unapply(x$0: Test.Foo): Some[(String, Int)] = scala.Some.apply[(String, Int)](scala.Tuple2.apply[String, Int](x$0.s, x$0.n));
      def apply(s: String, n: Int): Test.Foo = new Test.this.Foo(s, n)
    }
  }

