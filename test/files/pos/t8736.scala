// scalac: -feature -language:implicitConversions -language:higherKinds -language:-implicitConversions -Xfatal-warnings
// showing that multiple settings are respected, and explicit enablement has precedence
class X {
  def hk[M[_]] = ???

  implicit def imp(x: X): Int = x.hashCode
}
