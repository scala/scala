package tastytest

object ValueOfErasure {
  def reify[I <: Int](implicit I: ValueOf[I]): I = valueOf[I]
}
