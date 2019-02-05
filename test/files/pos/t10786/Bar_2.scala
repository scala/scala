package pkg {
  class C {
    class T1 extends Foo_1.StaticClass
    class T2 extends Foo_1.ProtectedStaticClass
    def test(): Unit = {
      val n1 = new Foo_1.StaticClass
      n1.x
      Foo_1.StaticClass.y
      val n2 = new Foo_1.ProtectedStaticClass
      n2.x
      Foo_1.ProtectedStaticClass.y
    }

    class I extends Foo_1 {
      class T1 extends Foo_1.StaticClass
      class T2 extends Foo_1.ProtectedStaticClass
      def test(): Unit = {
        val n1 = new Foo_1.StaticClass
        n1.x
        Foo_1.StaticClass.y
        val n2 = new Foo_1.ProtectedStaticClass
        n2.x
        Foo_1.ProtectedStaticClass.y
      }
    }
  }
}
