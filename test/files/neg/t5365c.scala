object C {
  trait Z
  final case class Q(i: Int) extends Z

  def unsealedTrait(z: Z) = z match {
    case Q(_) =>
  }

  def unsealedTraitWithCatchall(z: Z) = z match {
    case Q(_) =>
    case _ =>
  }

  def uncheckedUnsealedTrait(z: Z) = (z: @unchecked) match {
    case Q(_) =>
  }
}
