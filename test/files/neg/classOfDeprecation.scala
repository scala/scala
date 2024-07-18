//> using options -deprecation -Werror

@deprecated("no no!", "like, forever") class C
class ann(x: Any) extends annotation.Annotation
object T {
  val t = classOf[C]
  @ann(classOf[C]) def u = 1
}
