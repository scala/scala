//> using options -Werror
import scala.reflect.api.Universe

object Test {
  type SingletonUniverse = Universe with Singleton

  def foo[U <: SingletonUniverse](u: U)(typ: U#Type): List[U#Annotation] = typ match {
    case t: U#AnnotatedTypeApi => t.annotations // as a comparison, this wasn't emitting a warning
    case _                     => Nil
  }
}
