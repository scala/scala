object Test {
  class A {
    val d = 55

    def contrivedExample[A, B, C](a: A, b: B, c: C): Unit = a match {
      case b => println("matched b")
      case c => println("matched c")
      case d => println("matched d")
      case _ => println("matched neither")
    }

    def correctExample[A, B, C](a: A, b: B, c: C): Unit = a match {
      case `b` => println("matched b")
      case `c` => println("matched c")
      case `d` => println("matched d")
      case _ => println("matched neither")
    }

    def f[A](a: A) = {
      def g[B](b: B) = {
        def h[C](c: C) = a match {
          case b => 1
          case c => 2
          case _ => 3
        }
      }
    }
  }
}
