class Test {
  class A[T]
  class B[T](val a: A[T])

  case class CaseClass[T](x: T)

  def break(existB: B[_]) =
    CaseClass(existB.a) match { case CaseClass(_) => }
}

class Foo {
  trait Init[T]
  class ScopedKey[T] extends Init[T]

  trait Setting[T] {
    val key: ScopedKey[T]
  }

  case class ScopedKey1[T](val foo: Init[T]) extends ScopedKey[T]

  val scalaHome: Setting[Option[String]] = null
  val scalaVersion: Setting[String] = null

  def testPatternMatch(s: Setting[_]) {
    s.key match {
      case ScopedKey1(scalaHome.key | scalaVersion.key) => ()
    }
  }
}

class Test2 {
  type AnyCyclic = Execute[Task]#CyclicException[_]

  trait Task[T]

  trait Execute[A[_] <: AnyRef] {
    class CyclicException[T](val caller: A[T], val target: A[T])
  }

  def convertCyclic(c: AnyCyclic): String =
    (c.caller, c.target) match {
      case (caller: Task[_], target: Task[_]) => "bazinga!"
    }
}

