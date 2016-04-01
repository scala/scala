object Test extends App {
  val foos = (1 to 1000).toSeq
  try {
    foos.par.map(i => if (i % 37 == 0) throw new MultipleOf37Exception(i) else i)
    assert(false)
  } catch {
    case ex: MultipleOf37Exception =>
      assert(ex.getSuppressed.size > 0)
      assert(ex.getSuppressed.forall(_.isInstanceOf[MultipleOf37Exception]))
      assert(ex.i == 37)
      assert(ex.getSuppressed.map(_.asInstanceOf[MultipleOf37Exception].i).toList == List(74, 148, 259, 518))
    case _: Throwable =>
      assert(false)
  }
  class MultipleOf37Exception(val i: Int) extends RuntimeException
}
