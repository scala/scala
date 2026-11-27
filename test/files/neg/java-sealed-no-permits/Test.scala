//> using jvm 17+
//> using options -Werror
class Test {
  def missingC(s: S): Int = s match {
    case _: S.A => 0
    case _: S.B => 1
    case _: S.E => 2
  }
  def missingB(s: S): Int = s match {
    case _: S.A => 0
    case _: S.E => 2
    case _: S.C => 3
  }
  def sealedClass(t: T): Int = t match {
    case _: T.A => 0
  }
}
