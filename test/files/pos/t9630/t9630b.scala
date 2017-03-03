
class Test {
  def test(b: Base): Unit = b match {
    case Base_1(Some(_)) =>
    case Base_2(Nested_1(_)) =>
    case Base_2(Nested_2(_)) =>
  }
}
