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
