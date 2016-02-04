object Test {
  def testMethodLocalCaseClass {
    object MethodLocalWide
    case class MethodLocalWide(
                                f01: Int, f02: Int, f03: Int, f04: Int, f05: Int, f06: Int, f07: Int, f08: Int, f09: Int, f10: Int,
                                f11: Int, f12: Int, f13: Int, f14: Int, f15: Int, f16: Int, f17: Int, f18: Int, f19: Int, f20: Int,
                                f21: Int, f22: Int, f23: Int)

    val instance = MethodLocalWide(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val result = instance match {
      case MethodLocalWide(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) => true
      case _ => false
    }
    assert(result)
  }
  def main(args: Array[String]) {
    testMethodLocalCaseClass
  }
}
