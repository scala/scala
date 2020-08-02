import scala.language.implicitConversions

object Test {
  trait TC[A, B]

  implicit def mkTC[A, B]: TC[A, B] = null

  implicit def conversion[A, B](a: A)(implicit tc: TC[A, B]): B = ???

  val s: String = 1
}