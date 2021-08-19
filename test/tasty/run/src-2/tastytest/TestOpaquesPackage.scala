package tastytest.opaques {
  import tastytest.opaques.Offset
  import a.A

  class Test1 {
    import tastytest._

    def test(): Unit = {
      assert(A(Offset(10)) === A(Offset(10)))
    }

  }
}


package tastytest {

  object TestOpaquesPackage extends Suite("TestOpaquesPackage") {

    test(new opaques.Test1().test())
    test(a.Main.foo())

  }
}
