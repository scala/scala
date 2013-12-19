object Test {
  def testC {
    val f1 = (c: C) => c.value
    val f2 = (x: Int) => new C(x)
    val f3 = (c1: C) => (c2: C) => (c1, c2)
    val r1 = f2(2)
    val r2 = f2(2)
    val r3 = f3(r1)(r2)
    val result = f1(r3._2)
    assert(result == 2)
  }

  def testD {
    val f1 = (c: D) => c.value
    val f2 = (x: String) => new D(x)
    val f3 = (c1: D) => (c2: D) => (c1, c2)
    val r1 = f2("2")
    val r2 = f2("2")
    val r3 = f3(r1)(r2)
    val result = f1(r3._2)
    assert(result == "2")
  }

  def testE {
    val f1 = (c: E[Int]) => c.value
    val f2 = (x: Int) => new E(x)
    val f3 = (c1: E[Int]) => (c2: E[Int]) => (c1, c2)
    val r1 = f2(2)
    val r2 = f2(2)
    val r3 = f3(r1)(r2)
    val result = f1(r3._2)
    assert(result == 2)
  }

  def main(args: Array[String]) {
    testC
    testD
    testE
  }
}
