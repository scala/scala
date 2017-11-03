object Test {
  case class Tag0[T <: Singleton](a: T)
  Tag0("a").copy()

  case class Tag1[T](a: T)
  Tag1("a").copy()

  def foo[T <: Singleton](a: T = "a"): T = a
  val v0: "a" = foo()

  def bar(a: "a" = "a"): "a" = a
  val v1: "a" = bar()

  def baz[T](a: T = "a"): T = a
  val v2: "a" = baz()

  def id[T](a: T): T = a
  val v3: "a" = id("a")
}
