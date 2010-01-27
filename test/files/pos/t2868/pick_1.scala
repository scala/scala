class ann(s: String) extends StaticAnnotation
class pick {
  final val s = "bang!"
  @ann("bang!") def foo = 1
  @Jann(str = "bang!", inn = new Nest(1), arr = Array(1, 2)) def bar = 2
  @Jann(str = "bang!", inn = new Nest(1), arr = Array(1, 2)) def baz = 3
}
