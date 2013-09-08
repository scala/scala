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

final class ThreeStringExtract(val s: String) extends AnyVal {
  def isEmpty                                      = (s eq null) || (s == "")
  def get: (List[Int], Double, ThreeStringExtract) = ((s.length :: Nil, s.length.toDouble, this))
  def length                                       = s.length
  def lengthCompare(n: Int)                        = s.length compare n
  def apply(idx: Int): Char                        = s charAt idx
  def head: Char                                   = s charAt 0
  def tail: String                                 = s drop 1
  def drop(n: Int): ThreeStringExtract             = new ThreeStringExtract(s drop n)

  override def toString = s
}


object Bippy {
  def unapplySeq(x: Any): StringExtract = new StringExtract("" + x)
}
object TripleBippy {
  def unapplySeq(x: Any): ThreeStringExtract = new ThreeStringExtract("" + x)
}

object Test {
  def f(x: Any) = x match {
    case Bippy('B' | 'b', 'O' | 'o', 'B' | 'b', xs @ _*) => xs
    case _                                               => "nope"
  }

  def g(x: Any): String = x match {
    case TripleBippy(3 :: Nil, 3.0, 'b', chars @ _*)       => "1: " + chars
    case TripleBippy(5 :: Nil, 5.0, 'b' | 'B', chars @ _*) => "2: " + chars
    case TripleBippy(_, _, chars @ _*)                     => "3: " + chars
    case _                                                 => "nope"
  }

  def main(args: Array[String]): Unit = {
    println(f("Bobby"))
    println(f("BOBBY"))
    println(f("BoBoTheClown"))
    println(f("TomTomTheClown"))

    println(g("bob"))
    println(g("bobby"))
    println(g("BOBBY"))
    println(g("BOBO"))
    println(g("TomTomTheClown"))
  }
}
