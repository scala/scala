import scala.languageFeature.higherKinds

class M[A](a: A)
object M {
  implicit def m[MM[_], A]: MM[A] = ???
}
