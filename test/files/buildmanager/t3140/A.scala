class As {
  trait A {
      def foo(parents: String): A = {
        (() => parents)
        null
    }
  }
}
