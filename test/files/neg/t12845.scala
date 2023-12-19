//> using options -Werror -deprecation -Xlint

object O {
  @Deprecated def f = 0
}
class C { def g = O.f }
