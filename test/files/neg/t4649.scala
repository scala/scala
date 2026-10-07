
import annotation.tailrec

object neg {

  var sz = 3
  def remove(idx: Int) =
    if (idx >= 0 && idx < sz)
      sz -= 1
    else throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${sz-1})")

  // method contains no recursive calls
  @tailrec
  final def remove(idx: Int, count: Int): Unit =
    if (count > 0) {
      remove(idx) // at a glance, looks like a tailrec candidate, but must error in the end
                  // was: recursive call targeting a supertype
    }
}

object pos {

  var sz = 3
  def remove(idx: Int) =
    if (idx >= 0 && idx < sz)
      sz -= 1
    else throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${sz-1})")

  @tailrec final def remove(idx: Int, count: Int): Unit =
    if (count > 0) {
      remove(idx) // after rewrite, don't flag me as a leftover tailrec
      remove(idx, count-1)
    }
}
