//> using options -Xfatal-warnings -deprecation
import scala.annotation.StaticAnnotation

@deprecated("class!", "") class A extends StaticAnnotation

class B @deprecated("constructor!", "") extends StaticAnnotation

@A class C

@B class D   // should warn

trait T {
  def foo = new B
}
