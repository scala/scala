abstract final class List[a] with {
  def ::(x: a): List[a] = cons(x, this);

  def head: a = match {
    case [] => error("[].head");
    case x :: xs => x
  }

  def tail: List[a] = match {
    case [] => error("[].tail");
    case x :: xs => xs
  }

  def isEmpty = match {
    case [] => False;
    case _ :: _ => True;
  }

  def length: Int = match {
    case [] => 0;
    case x :: xs => 1 + xs.length;
  }

  def ::: (that: List[a]): List[a] = match {
    case [] => that;
    case x :: xs => x :: xs ::: that
  }

  def append(x: a): List[a] = this ::: x :: [];

  def map[b](f: (a)b): List[b] = match {
    case [] => [];
    case x :: xs => f(x) :: (xs map f)
  }

  def flatMap[b](f: (a)List[b]): List[b] = match {
    case [] => [];
    case x :: xs => f(x) ::: (xs flatMap f)
  }

  def filter(p: (a)Boolean): List[a] = match {
    case [] => [];
    case x :: xs => if (p(x)) x :: (xs filter p) else xs filter p
  }

  def foldl[b](f: (b, a)b)(z: b): b = match {
    case [] => z;
    case x :: xs => (xs foldl f)(f(z, head))
  }

  def foldr[b](f: (a, b)b)(z: b): b = match {
    case [] => z;
    case x :: xs => f(x, (xs foldr f)(z))
  }

  def foldl1(f: (a, a)a): a = match {
    case [] => error("[].foldl1");
    case x :: xs => (xs foldl f)(x)
  }

  def foldr1(f: (a, a)a): a = match {
    case [] => error("[].foldr1");
    case x :: [] => x;
    case x :: xs => f(x, (xs foldr1 f))
  }

  def forall(p: (a)Boolean): Boolean = match {
    case [] => True;
    case x :: xs => p(x) && (xs forall p)
  }

  def exists(p: (a)Boolean): Boolean = match {
    case [] => False;
    case x :: xs => p(x) || (xs exists p)
  }

  def take(n: Int): List[a] = match {
    case [] => [];
    case x :: xs => if (n == 0) [] else x :: (xs take (n - 1))
  }

  def drop(n: Int): List[a] = match {
    case [] => [];
    case x :: xs => if (n == 0) this else xs drop (n - 1)
  }

  def takeWhile(p: (a)Boolean): List[a] = match {
    case [] => [];
    case x :: xs => if (p(x)) x :: (xs takeWhile p) else []
  }

  def dropWhile(p: (a)Boolean): List[a] = match {
    case [] => [];
    case x :: xs => if (p(x)) (xs dropWhile p) else this
  }

  def init: List[a] = match {
    case [] => error("[].init");
    case x :: [] => [];
    case x :: xs => xs.init
  }

  def last: a = match {
    case [] => error("[].last");
    case x :: [] => x;
    case x :: xs => xs.last
  }

  def reverse: List[a] = {
    def snoc(xs: List[a], x: a) = x :: xs;
    foldl(snoc)([])
  }

  def zip[b](that: List[b]): List[(a,b)] = (this, that) match {
    case (x :: xs, y :: ys) => (x, y) :: (xs zip ys)
    case _ => []
  }

  override def toString(): String = "[" + mkString(",") + "]";
  def mkString(sep: String): String = match {
    case [] => "";
    case x :: [] => x.toString();
    case x :: xs => x.toString() + sep + xs.toString()
  }
}

def error[a](x: String):a = (new java.lang.RuntimeException(x)).throw;

case class Nil[b] extends List[b];
case class ::_class[b](x: b)(xs: List[b]) extends List[b];
def cons[a](x: a, xs: List[a]) = ::_class(x)(xs);
def nil[a] = new Nil[a];







