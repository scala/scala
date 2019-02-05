import language.dynamics

trait Typeclass[T]
class TCInstance
object TCInstance {
  implicit object instance extends Typeclass[TCInstance]
}
class Dyn extends Dynamic {
  def updateDynamic[T: Typeclass](f: String)(t: T) = println(s"$f: $t")
}
object Dyn {
  new Dyn().foo = new TCInstance
}
