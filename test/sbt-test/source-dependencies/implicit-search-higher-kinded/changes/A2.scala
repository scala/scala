import scala.languageFeature.higherKinds
trait A
object A {
  implicit def m[MM[_], A]: MM[A] = ???
}
