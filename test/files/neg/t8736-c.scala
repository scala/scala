// scalac: -feature -language:-higherKinds,_ -Xfatal-warnings
// scalac: -feature -language:-higherKinds,_ -Xfatal-warnings
// showing that wildcard doesn't supersede explicit disablement
class X {
  def hk[M[_]] = ???

  implicit def imp(x: X): Int = x.hashCode
}
