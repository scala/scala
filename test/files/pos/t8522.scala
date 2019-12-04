trait IterateeT[F[_]]
trait StepT[F[_]]

class Test {
  def iterateeT[F[_]](s: F[StepT[F]]): IterateeT[F] = ???

  def fail[M[+_]]: IterateeT[M] = {
    val back: M[StepT[M]] = ???
    iterateeT(back) // fails after SI-2066 fix
  }

  def okay1[M[_]]: IterateeT[M] = {
    val back: M[StepT[M]] = ???
    iterateeT(back) // okay if M is invariant
  }

  def okay2[M[_]]: IterateeT[M] = {
    val back: M[StepT[M]] = ???
    iterateeT[M](back) // okay if type args are explicit
  }
}
