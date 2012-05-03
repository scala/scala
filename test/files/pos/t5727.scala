
/*
 * We like operators, bar none.
 */
object Test {

  trait SomeInfo
  case object NoInfo extends SomeInfo

  sealed abstract class Res[+T]
  case object NotRes extends Res[Nothing]


  abstract class Base[+T] {
    def apply(f: String): Res[T]
    // 'i' crashes the compiler, similarly if we use currying
    //def |[U >: T](a: => Base[U], i: SomeInfo = NoInfo): Base[U] = null
    def bar[U >: T](a: => Base[U], i: SomeInfo = NoInfo): Base[U] = null
  }

  implicit def fromStringToBase(a: String): Base[String] = new Base[String] { def apply(in: String) = NotRes }

  // bug
  //def Sample: Base[Any] = ( rep("foo" | "bar") | "sth")
  def Sample: Base[Any] = ( rep("foo" bar "bar") bar "sth")

  def rep[T](p: => Base[T]): Base[T] = null // whatever

  def main(args: Array[String]) {
  }
}
