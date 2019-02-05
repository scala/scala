object repro {
  import scala.reflect.runtime.universe._

  trait +[L, R]

  case class Atomic[V](val name: String)
  object Atomic {
    def apply[V](implicit vtt: TypeTag[V]): Atomic[V] = Atomic[V](vtt.tpe.typeSymbol.name.toString)
  }

  case class Assign[V, X](val name: String)
  object Assign {
    def apply[V, X](implicit vtt: TypeTag[V]): Assign[V, X] = Assign[V, X](vtt.tpe.typeSymbol.name.toString)
  }

  trait AsString[X] {
    def str: String
  }
  object AsString {
    implicit def atomic[V](implicit a: Atomic[V]): AsString[V] =
      new AsString[V] { val str = a.name }
    implicit def assign[V, X](implicit a: Assign[V, X], asx: AsString[X]): AsString[V] =
      new AsString[V] { val str = asx.str }
    implicit def plus[L, R](implicit asl: AsString[L], asr: AsString[R]): AsString[+[L, R]] =
      new AsString[+[L, R]] { val str = s"(${asl.str}) + (${asr.str})" }
  }

  trait X
  implicit val declareX = Atomic[X]
  trait Y
  implicit val declareY = Atomic[Y]
  trait Z
  implicit val declareZ = Atomic[Z]

  trait Q
  implicit val declareQ = Assign[Q, (X + Y) + Z]
  trait R
  implicit val declareR = Assign[R, Q + Z]

  implicitly[AsString[X]]
  implicitly[AsString[X + Y]]
  implicitly[AsString[Q]]
  implicitly[AsString[R]]
}
