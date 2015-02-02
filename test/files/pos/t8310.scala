trait Comparinator[T] { def compare(a: T, b: T): Int }

object TestOkay {
  def sort(x: Comparinator[_ >: String]) = ()
  sort((a: String, b: String) => a.compareToIgnoreCase(b))
}

object TestOkay2 {
  def sort[T](x: Comparinator[_ >: T]) = ()
  sort((a: String, b: String) => a.compareToIgnoreCase(b))
}

object TestOkay3 {
  def sort[T](xs: Option[T], x: Comparinator[_ >: T]) = ()
  sort(Some(""), (a: String, b: String) => a.compareToIgnoreCase(b))
}

object TestKoOverloaded {
  def sort[T](xs: Option[T]) = ()
  def sort[T](xs: Option[T], x: Comparinator[_ >: T]) = ()
  sort(Some(""), (a: String, b: String) => a.compareToIgnoreCase(b))
}
