package helloworld

object HelloWorld {
  inline val msg1 = "Hello, World!"
  def acceptsOnlyMsg1(m: msg1.type): String = m + m
  def higherBounded2[T <: List[? <: Int]](f: T): T = f
  def higherBounded3[T <: List[List[? <: Int]]](f: T): T = f
  def higherBounded4[T <: Either[? <: Int, String]](f: T): T = f
  def higherBounded5[F[+_]] = ???
  def higherBounded6[F[-_]] = ???
  inline def inlineMethod(inline i: Int): Int = i
}
