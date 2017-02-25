object Test {

  trait ~>[A[_], B[_]] {
    def apply[I](fa: A[I]): B[I]
  }

  // HIGHER KINDED GADT

  sealed trait GADTK[A[_], I]
  final case class MemberK[A[_]](i: Int) extends GADTK[A, Int]

  def doesNotCompile[A[_]]:
      (({ type λ[α] = GADTK[A, α] })#λ ~> ({ type λ[α] = GADTK[A, α] })#λ) =
    new (({ type λ[α] = GADTK[A, α] })#λ ~> ({ type λ[α] = GADTK[A, α] })#λ) {
      def apply[I](v: GADTK[A, I]): GADTK[A, I] = v match {
        case MemberK(i) => MemberK(i)
      }
    }

  class CompilesFine[A[_]]
      extends (({ type λ[α] = GADTK[A, α] })#λ ~> ({ type λ[α] = GADTK[A, α] })#λ) {
    def apply[I](v: GADTK[A, I]): GADTK[A, I] = v match {
      case MemberK(i) => MemberK(i)
    }
  }

  // SIMPLE GADT

  sealed trait GADT[A, I]
  final case class Member[A](i: Int) extends GADT[A, Int]

  def compilesFine[A]:
      (({ type λ[α] = GADT[A, α] })#λ ~> ({ type λ[α] = GADT[A, α] })#λ) =
    new (({ type λ[α] = GADT[A, α] })#λ ~> ({ type λ[α] = GADT[A, α] })#λ) {
      def apply[I](v: GADT[A, I]): GADT[A, I] = v match {
        case Member(i) => Member(i)
      }
    }
}
