package t12349

package pkg {

  object t12349c {

    class Inner12349c extends t12349a {
                         override def a1(): Unit = println("Inner12349c#a1()")
      protected          override def a2(): Unit = println("Inner12349c#a2()") // weaker access privileges
        private          override def a3(): Unit = println("Inner12349c#a3()") // weaker access privileges
      protected[t12349c] override def a4(): Unit = println("Inner12349c#a4()") // weaker access privileges
        private[t12349c] override def a5(): Unit = println("Inner12349c#a5()") // weaker access privileges
      protected[pkg]     override def a6(): Unit = println("Inner12349c#a6()") // weaker access privileges
        private[pkg]     override def a7(): Unit = println("Inner12349c#a7()") // weaker access privileges
      protected[Inner12349c] override def a8(): Unit = println("Inner12349c#a8()") // weaker access privileges
        private[Inner12349c] override def a9(): Unit = println("Inner12349c#a9()") // weaker access privileges
      protected[this]    override def aA(): Unit = println("Inner12349c#aA()") // weaker access privileges
        private[this]    override def aB(): Unit = println("Inner12349c#aB()") // weaker access privileges

                         override def b1(): Unit = println("Inner12349c#b1()")
      protected          override def b2(): Unit = println("Inner12349c#b2()")
        private          override def b3(): Unit = println("Inner12349c#b3()") // weaker access privileges
      protected[t12349c] override def b4(): Unit = println("Inner12349c#b4()")
        private[t12349c] override def b5(): Unit = println("Inner12349c#b5()") // weaker access privileges
      protected[pkg]     override def b6(): Unit = println("Inner12349c#b6()")
        private[pkg]     override def b7(): Unit = println("Inner12349c#b7()") // weaker access privileges
      protected[Inner12349c] override def b8(): Unit = println("Inner12349c#b8()") // [#12349] - not fixed by PR #9525
        private[Inner12349c] override def b9(): Unit = println("Inner12349c#b9()") // weaker access privileges
      protected[this]    override def bA(): Unit = println("Inner12349c#bA()") // [#12349] - not fixed by PR #9525
        private[this]    override def bB(): Unit = println("Inner12349c#bB()") // weaker access privileges

                         override def c1(): Unit = println("Inner12349c#c1()") // overrides nothing (invisible)
      protected          override def c2(): Unit = println("Inner12349c#c2()") // weaker access privileges
        private          override def c3(): Unit = println("Inner12349c#c3()") // weaker access privileges
      protected[t12349c] override def c4(): Unit = println("Inner12349c#c4()") // weaker access privileges
        private[t12349c] override def c5(): Unit = println("Inner12349c#c5()") // weaker access privileges
      protected[pkg]     override def c6(): Unit = println("Inner12349c#c6()") // weaker access privileges
        private[pkg]     override def c7(): Unit = println("Inner12349c#c7()") // weaker access privileges
      protected[Inner12349c] override def c8(): Unit = println("Inner12349c#c8()") // weaker access privileges
        private[Inner12349c] override def c9(): Unit = println("Inner12349c#c9()") // weaker access privileges
      protected[this]    override def cA(): Unit = println("Inner12349c#cA()") // weaker access privileges
        private[this]    override def cB(): Unit = println("Inner12349c#cB()") // weaker access privileges

                         override def d1(): Unit = println("Inner12349c#d1()") // overrides nothing
      protected          override def d2(): Unit = println("Inner12349c#d2()") // overrides nothing
        private          override def d3(): Unit = println("Inner12349c#d3()") // overrides nothing
      protected[t12349c] override def d4(): Unit = println("Inner12349c#d4()") // overrides nothing
        private[t12349c] override def d5(): Unit = println("Inner12349c#d5()") // overrides nothing
      protected[pkg]     override def d6(): Unit = println("Inner12349c#d6()") // overrides nothing
        private[pkg]     override def d7(): Unit = println("Inner12349c#d7()") // overrides nothing
      protected[Inner12349c] override def d8(): Unit = println("Inner12349c#d8()") // overrides nothing
        private[Inner12349c] override def d9(): Unit = println("Inner12349c#d9()") // overrides nothing
      protected[this]    override def dA(): Unit = println("Inner12349c#dA()") // overrides nothing
        private[this]    override def dB(): Unit = println("Inner12349c#dB()") // overrides nothing
    }

  }

}
