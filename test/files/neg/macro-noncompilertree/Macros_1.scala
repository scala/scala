object Macros {
  def macro foo = scala.reflect.mirror.Literal(scala.reflect.mirror.Constant(2))
}