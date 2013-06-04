import scala.reflect.ClassTag

class C1(val n: Int) extends AnyVal
class C2(val n: Int) extends AnyRef

object Test {
  type F1 = C1
  type F2 = C2

  def main(args: Array[String]): Unit = {
    println(implicitly[ClassTag[C1]])
    println(implicitly[ClassTag[C2]])
    println(implicitly[ClassTag[F1]])
    println(implicitly[ClassTag[F2]])
  }
}
