package example


trait PartialFunction[-A, +B] extends Function1[A,B] { self =>
  def isDefinedAt(x: A): Boolean
  def compose[CLASH](k: PartialFunction[CLASH, A]): PartialFunction[CLASH, B] = null
}

trait Function1[-T1, +R] extends AnyRef { self =>
  def apply(v1: T1): R
  def compose[A](g: Function1[A, T1]): Function1[A , R] = null
}

abstract class AbstractPartialFunction[-T1, +CLASH] extends Function1[T1, CLASH] with PartialFunction[T1, CLASH] { self =>
  def apply(x: T1): CLASH = ???
}
