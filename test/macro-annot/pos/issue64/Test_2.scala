package issue64

object Test extends App {
  class ann extends scala.annotation.StaticAnnotation
  def Foo = ???
  @ann trait Foo[T]
  def Bar = ???
  @pkg.identity trait Bar[T]
}
