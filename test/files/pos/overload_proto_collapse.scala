
class Test {
  def prepended[B >: Char](elem: B): String = ???
  def prepended(c: Char): String = ???

  def +:[B >: Char](elem: B): String = prepended(elem)
}


trait DurationConversions {
  trait Classifier[C] { type R }

  def days: Int = ???
  def days[C](c: C)(implicit ev: Classifier[C]): ev.R = ???

  def day[C](c: C)(implicit ev: Classifier[C]): ev.R = days(c)
}


trait AnonMatch {
  trait MapOps[K, +V, +CC[_, _]] {
    def map[K2, V2](f: ((K, V)) => (K2, V2)): CC[K2, V2] = ???
    def map[K2 <: AnyRef, V2](f: ((K with AnyRef, V)) => (K2, V2)): MapOps[K2, V2, Map] = ???
  }

  (??? : MapOps[String, Int, Map]).map{ case (k,v) => ??? }
}


trait FBounds {
  def f[A](x: A) = 11;
  def f[A <: Ordered[A]](x: Ordered[A]) = 12;

  f(1)
}

// Don't collapse A and Tree[A]. Naively replacing type params with ? gives ? and Tree[?],
// which are equal because wildcard equals whatever
// example from specs2
class Trees { outer =>
  trait Tree[B]

  def clean[A](t: Tree[Option[A]]): Tree[A] =
    prune(t, (a: Option[A]) => a).getOrElse(??? : Tree[A])

  def prune[A, B](t: Tree[A], f: A => Option[B]): Option[Tree[B]] = ???
  def prune[A](t: Tree[A], f: Tree[A] => Option[A])(implicit initial: A): Tree[A] = ???
}


// From gigahorse
abstract class Sam[A] { def apply(a: String): A }

class GigaHorse {
  def map[A](f: String => A): A = map(new Sam[A] { def apply(a: String): A = f(a) })
  def map[A](f: Sam[A]): A = ???
}