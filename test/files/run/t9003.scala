object Single {
  var i = 0
  def isEmpty = false
  def get = i
  def unapply(a: Single.type) = this
}

object Product {
  var i = 0
  def _1: Int = i
  def _2: String = ???
  def productArity = 2
  def unapply(a: Product.type) = this
  def isEmpty = false
  def get: this.type = this
}

object Sequence {
  var i = 0
  def apply(n: Int): Int = i
  def length = 2
  def unapplySeq(a: Sequence.type) = this
  def isEmpty = false
  def get = this
}

object Test {
  def main(args: Array[String]): Unit = {
    def assertZero(i: Int) = assert(i == 0)

    Single match {
      case Single(i) =>
        Single.i = 1
        assertZero(i) // fails under -optimize
    }

    Product match {
      case Product(i, _) =>
        Product.i = 1
        assertZero(i) // fails under -optimize
    }

    Sequence match {
      case Sequence(i, _ @ _*) =>
        Sequence.i = 1
        assertZero(i) // okay
    }

    Sequence.i = 0
    Sequence match {
      case Sequence(_, i) =>
        Sequence.i = 1
        assertZero(i) // okay
    }

    val buffer = collection.mutable.Buffer(0, 0)
    buffer match {
      case Seq(_, i) =>
        buffer(1) = 1
        assertZero(i) // failed
    }

    case class CaseSequence(as: Int*)
    val buffer1 = collection.mutable.Buffer(0, 0)
    CaseSequence(buffer1: _*) match {
      case CaseSequence(_, i) =>
        buffer1(1) = 1
        assertZero(i) // failed
    }
  }
}
