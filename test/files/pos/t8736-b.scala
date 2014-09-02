// scalac: -feature -language:_ -Xfatal-warnings
// showing that all are set
class X {
  def hk[M[_]] = ???

  implicit def imp(x: X): Int = x.hashCode
}
