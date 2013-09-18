

trait Base[B] {
  def data: AnyRef;
}


class M[@specialized(Int) A]


class Sub3[@specialized(Int) B](override val data: M[B]) extends Base[B] {
  assert(data != null)
}


object Test {
  def main(args: Array[String]) {
    new Sub3[Int](new M[Int])
  }
}
