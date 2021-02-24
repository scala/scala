package javapkg

object t12349b {

  class Inner12349b extends t12349a {
                       override def a1(): Unit = println("Inner12349b#a1()")
    protected          override def a2(): Unit = println("Inner12349b#a2()") // weaker access privileges
      private          override def a3(): Unit = println("Inner12349b#a3()") // weaker access privileges
    protected[t12349b] override def a4(): Unit = println("Inner12349b#a4()") // weaker access privileges
      private[t12349b] override def a5(): Unit = println("Inner12349b#a5()") // weaker access privileges
    protected[javapkg] override def a6(): Unit = println("Inner12349b#a6()") // weaker access privileges
      private[javapkg] override def a7(): Unit = println("Inner12349b#a7()") // weaker access privileges
    protected[this]    override def a8(): Unit = println("Inner12349b#a8()") // weaker access privileges
      private[this]    override def a9(): Unit = println("Inner12349b#a9()") // no effect on private[this] (will it be fixed?)

                       override def b1(): Unit = println("Inner12349b#b1()")
    protected          override def b2(): Unit = println("Inner12349b#b2()")
      private          override def b3(): Unit = println("Inner12349b#b3()") // weaker access privileges
    protected[t12349b] override def b4(): Unit = println("Inner12349b#b4()")
      private[t12349b] override def b5(): Unit = println("Inner12349b#b5()") // weaker access privileges
    protected[javapkg] override def b6(): Unit = println("Inner12349b#b6()")
      private[javapkg] override def b7(): Unit = println("Inner12349b#b7()") // weaker access privileges
    protected[this]    override def b8(): Unit = println("Inner12349b#b8()") // protected[this] seems to be another problem, so leave it as it is
      private[this]    override def b9(): Unit = println("Inner12349b#b9()") // no effect on private[this] (will it be fixed?)

                       override def c1(): Unit = println("Inner12349b#c1()")
    protected          override def c2(): Unit = println("Inner12349b#c2()") // weaker access privileges
      private          override def c3(): Unit = println("Inner12349b#c3()") // weaker access privileges
    protected[t12349b] override def c4(): Unit = println("Inner12349b#c4()") // weaker access privileges
      private[t12349b] override def c5(): Unit = println("Inner12349b#c5()") // weaker access privileges
    protected[javapkg] override def c6(): Unit = println("Inner12349b#c6()")
      private[javapkg] override def c7(): Unit = println("Inner12349b#c7()")
    protected[this]    override def c8(): Unit = println("Inner12349b#c8()") // weaker access privileges
      private[this]    override def c9(): Unit = println("Inner12349b#c9()") // no effect on private[this] (will it be fixed?)

                       override def d1(): Unit = println("Inner12349b#d1()") // overrides nothins
    protected          override def d2(): Unit = println("Inner12349b#d2()") // overrides nothins
      private          override def d3(): Unit = println("Inner12349b#d3()") // overrides nothins
    protected[t12349b] override def d4(): Unit = println("Inner12349b#d4()") // overrides nothins
      private[t12349b] override def d5(): Unit = println("Inner12349b#d5()") // overrides nothins
    protected[javapkg] override def d6(): Unit = println("Inner12349b#d6()") // overrides nothins
      private[javapkg] override def d7(): Unit = println("Inner12349b#d7()") // overrides nothins
    protected[this]    override def d8(): Unit = println("Inner12349b#d8()") // overrides nothins
      private[this]    override def d9(): Unit = println("Inner12349b#d9()") // overrides nothins
  }

}
