abstract class TypeAnnotations[@specialized R] {
  @specialized val x = 10
  @specialized type T

  def compose[@specialized A](x: A, y: R): A = {
    val y: A = x
    x
  }
}