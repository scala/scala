package helloworld

object HelloWorld {
  inline val msg: String = "Hello, World!"
  final val msg1 = "Hello, World!"
  def acceptsOnlyMsg1(m: msg1.type): String = m + m
  def higherBounded2[T <: List[_ <: Int]](f: T): T = f
  def higherBounded3[T <: List[List[_ <: Int]]](f: T): T = f
  def higherBounded4[T <: Either[_ <: Int, String]](f: T): T = f
  def higherBounded5[F[+_]] = ???
  def higherBounded6[F[-_]] = ???
}
