object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    assert(Test_2.acceptD(Test_1.mD(new c.D)) == 2)
    assert(Test_2.acceptE(Test_1.mE(new C.E)) == 2)
    assert(Test_2.acceptG(Test_1.mG(new C.F.G)) == 2)
  }
}
