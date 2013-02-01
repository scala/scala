import scala.reflect.runtime.universe._

class A {
  def f[T: TypeTag] = typeOf[T] match { case TypeRef(_, _, args) => args }
}
