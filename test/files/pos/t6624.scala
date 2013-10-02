sealed trait KList[+M[_]]

case class KCons[M[_], +T <: KList[M]](
  tail: T
) extends KList[M]

case class KNil[M[_]]() extends KList[M]

object Test {
  val klist: KCons[Option, KCons[Option, KCons[Option, KNil[Nothing]]]] = ???

  // crashes with
  // "Exception in thread "main" scala.reflect.internal.Types$TypeError: value _1 is not a member
  // of KCons[Option,KCons[Option,KNil[Nothing]]]"
  klist match {
   case KCons(KCons(KCons(_))) =>
  }

  // fails with a similar message as an error, rather than a crash.
  klist match {
    case KCons(KCons(_)) =>
  }

  // succeeds
  klist match {
    case KCons(_) =>
  }
}