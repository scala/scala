package scala.runtime

/**
 * Constant functions, used internally to translate anonymous functions and
 * by-name arguments that do not access the arguments or depend on deferred execution.
 */
@SerialVersionUID(0L)
final class Const0[@specialized(Specializable.Primitives) +R](r: R) extends (() => R) with Serializable {
  def apply(): R = r
}

@SerialVersionUID(0L)
final class Const1[@specialized(Specializable.Primitives) +R](r: R) extends ((Any) => R) with Serializable {
  def apply(a: Any): R = r
}

@SerialVersionUID(0L)
final class Const2[@specialized(Specializable.Primitives) +R](r: R) extends ((Any, Any) => R) with Serializable {
  def apply(a: Any, b: Any): R = r
}

@SerialVersionUID(0L)
final class Const3[@specialized(Specializable.Primitives) +R](r: R) extends ((Any, Any, Any) => R) with Serializable {
  def apply(a: Any, b: Any, c: Any): R = r
}
