trait Foo[A, B]
object Foo {
  type Bar[A] = Foo[A, _]
}

trait Base[M[_]] {
  def method(in: M[_]): Unit
}

class Concrete extends Base[Foo.Bar] {
  def method(in: Foo.Bar[_]): Unit = {}
}

trait Template[M[_]] {
  def toBeImplemented: Base[M]
  def mark[A]: M[A]

  def method2(): Unit = {
    toBeImplemented.method(mark[Nothing])
  }
}

class Impl extends Template[Foo.Bar] {
  def toBeImplemented: Base[Foo.Bar] = new Concrete
  def mark[A]: Foo.Bar[A] = new Foo[A, Nothing] {}
}

object Test {
  def main(args: Array[String]): Unit =
    (new Impl).method2()
}
