import scala.reflect.ClassTag

object Test {
  def main(args: Array[String]): Unit = {
    assert(apply[Double](1d) == classOf[Array[Double]])
  }

  def apply[D <: Double: ClassTag](d: D): Class[_] = {
    Array.apply[D](d).getClass
  }
}
