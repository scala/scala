abstract class AbsFun[@specialized R] {
//  def andThen[B](x: B): B

  def compose[A](x: A, y: R): A = {
    val y: A = x
    x
  }
}
