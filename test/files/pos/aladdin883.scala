import scala.language.implicitConversions

trait Foo[A] {
  implicit def convert(a : A) : Ordered[A];
  class Filter(f : A => Boolean) extends Foo[A] {
    implicit def convert(a : A) = Foo.this.convert(a);
  }
  class Range(x : A, y : A) extends Filter(a => {
    (a).compare(x) >= 0 && (a).compare(y) < 0;
  }) {}
}
