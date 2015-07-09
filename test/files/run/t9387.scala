class G[T]
object G {
  def v[T](x: T): G[T] = null
}

class A[T]
object A {
  def apply[T](x: => G[T]): A[T] = null
}

object T {
  A[Unit](G.v(() => ())) // Was VerifyError
}

object Test {
  def main(args: Array[String]): Unit = {
    T
  }

}