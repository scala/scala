package tastytest

object TestDependentExtension {
  import DependentExtension._

  def test = {
    val box = new Box {
      type Repr = Int
      val value: Int = 23
    }
    val res = implicitly[DependentExtension].extract(box)(_ + 1)
    assert(res == 24)
  }

}
