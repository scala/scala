//> using options -Werror
import scala.reflect.api.Universe

object Test {
  type SingletonUniverse = Universe with Singleton

  def foo[U <: SingletonUniverse](u: U)(typ: u.Type): List[u.Annotation] = typ match {
    case t: u.AnnotatedTypeApi => t.annotations // was: "The outer reference in this type test cannot be checked at run time."
    case _                     => Nil
  }
}
