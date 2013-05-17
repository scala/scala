trait Trait[-A, +B, C] {
  def ok() = { // ok
    object O {
      private def f0(x: A): A = ???
      def f1(x: A): B = ???
      def f2(x: A): C = ???
      private def f3(x: B): A = ???
      private def f4(x: B): B = ???
      private def f5(x: B): C = ???
      private def f6(x: C): A = ???
      def f7(x: C): B = ???
      def f8(x: C): C = ???
    }
    O
  }

  def fail1() = { object O { def f0(x: A): A = ??? } ; O } // fail
  def fail2() = { object O { def f0(x: B): A = ??? } ; O } // fail
  def fail3() = { object O { def f0(x: B): B = ??? } ; O } // fail
  def fail4() = { object O { def f0(x: B): C = ??? } ; O } // fail
  def fail5() = { object O { def f0(x: C): A = ??? } ; O } // fail

  def fail6() = { // fail
    trait O0 {
      def f0(x: A): A = ???
      def f1(x: A): B = ???
      def f2(x: A): C = ???
    }
    object O1 extends O0
    O1
  }
  def fail7() = { // fail
    trait O0 {
      def f0(x: A): A = ???
      def f1(x: A): B = ???
      def f2(x: A): C = ???
    }
    new O0 { }
  }
}
