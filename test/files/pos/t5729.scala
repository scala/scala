trait T[X]
object Test {
  def join(in: Seq[T[_]]): Int = ???
  def join[S](in: Seq[T[S]]): String = ???
  join(null: Seq[T[_]])
}