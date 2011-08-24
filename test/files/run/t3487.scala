trait Bippy {
  def bippy = 5
}

class Test extends Bippy {
  def f1 = 55
}

object Test extends Test {
  def dingus = bippy
  def main(args: Array[String]): Unit = {
    assert(bippy + f1 == 110)
  }
  override def bippy = 55
}
