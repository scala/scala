abstract class AbsIterator {
  type T
  def hasNext: Boolean
  def next: T
}

trait RichIterator extends AbsIterator {
  def foreach(f: T => Unit) {
    while (hasNext) f(next)
  }
}

class StringIterator(s: String) extends AbsIterator {
  type T = Char
  private var i = 0
  def hasNext = i < s.length()
  def next = { val x = s.charAt(i); i += 1; println("next: " + x); x }
}

trait SyncIterator extends AbsIterator {
  abstract override def hasNext: Boolean =
    synchronized(super.hasNext)
  abstract override def next: T =
    synchronized {
      println("<sync>"); val x = super.next; println("</sync>"); x
    }
}
trait LoggedIterator extends AbsIterator {
  abstract override def next: T = {
    val x = super.next; println("log: " + x); x
  }
}
class Iter2(s: String) extends StringIterator(s)
               with SyncIterator with LoggedIterator;
object Test {
  def main(args: Array[String]) {
    class Iter extends StringIterator(args(0)) with RichIterator with SyncIterator with LoggedIterator
    val iter = new Iter
    iter foreach Console.println
  }
}
