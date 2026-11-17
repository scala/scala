// The default of `codec` is typed while `codec` does not carry `@defaultArg` yet.
class key[T](k: T, @infer codec: Ordering[T] = infer.value) extends scala.annotation.StaticAnnotation
class B { @key[Int](1) def f = 1 }
