
class Y[T](val i: Option[T]) extends AnyVal {
   def q: List[T] = {
      lazy val e: List[T] = i.toList
      e
   }
}

class YA[T](val i: Option[T]) {
   def q: List[T] = {
      lazy val e: List[T] = i.toList
      e
   }
}


class Z[T](val i: Option[T]) {
  def q: List[T] = Z.extensionQ(this)
  def qq: List[T] = {
    lazy val e: List[T] = i.toList
    e
  }
}

object Z {
  def extensionQ[T](_this: Z[T]): List[T] = {
    lazy val e: List[T] = _this.i.toList
    e
  }
}
