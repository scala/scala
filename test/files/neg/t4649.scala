
import annotation.tailrec

object Test {

  var sz = 3
  def remove(idx: Int) =
    if (idx >= 0 && idx < sz)
      sz -= 1
    else throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${sz-1})")

  @tailrec final def remove(idx: Int, count: Int): Unit =
    if (count > 0) {
      remove(idx) // at a glance, looks like a tailrec candidate, but must error in the end
    }

}
