// scala: the signature in the abstract class will use the
// upper bound as return type, which for us will be Integer
// since primitives can't appear in bounds.
abstract class AC[T <: Int] {
  def f(): T
}
class Bippy extends AC[Int] {
  def f(): Int = 5
}