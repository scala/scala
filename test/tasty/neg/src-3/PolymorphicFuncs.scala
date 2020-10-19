package tastytest

object PolymorphicFuncs {
  val id: [T] => (t: T) => T = [T] => (t: T) => t
  def takesId(f: [T] => (t: T) => T): Int = f(1)

  class PolyBox[F <: [T] => (t: T) => T] {
    def takesId(f: F) = f(1)
  }

}
