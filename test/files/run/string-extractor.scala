final class StringExtract(val s: String) extends AnyVal {
  def isEmpty                     = (s eq null) || (s == "")
  def get                         = this
  def length                      = s.length
  def lengthCompare(n: Int)       = s.length compare n
  def apply(idx: Int): Char       = s charAt idx
  def head: Char                  = s charAt 0
  def tail: String                = s drop 1
  def drop(n: Int): StringExtract = new StringExtract(s drop n)

  override def toString = s
}

object Bippy {
  def unapplySeq(x: Any): StringExtract = new StringExtract("" + x)
}

object Test {
  def f(x: Any) = x match {
    case Bippy('B' | 'b', 'O' | 'o', 'B' | 'b', xs @ _*) => xs
    case _                                               => "nope"
  }

  def main(args: Array[String]): Unit = {
    println(f("Bobby"))
    println(f("BOBBY"))
    println(f("BoBoTheClown"))
    println(f("TomTomTheClown"))
  }
}
