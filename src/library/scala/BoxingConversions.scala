package scala
abstract class BoxingConversions[Boxed, Unboxed] {
  def box(x: Unboxed): Boxed
  def unbox(x: Boxed): Unboxed
}
