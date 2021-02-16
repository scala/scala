import scala.reflect.ClassTag

object Test {
  def main(args: Array[String]): Unit = {
    assert(apply[String]("") == classOf[Array[String]])
    assert(apply[Double](1d) == classOf[Array[Double]])
  }

  def apply[@specialized(Double) C: ClassTag](c: C): Class[_] = {
    Array(c).getClass
  }
}
