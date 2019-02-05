import scala.language.higherKinds

trait ~>[A[_], B[_]] {
  def apply[I](fa: A[I]): B[I]
}

sealed trait GADTK[A[_], I]
final case class MemberK[A[_]](i: Int) extends GADTK[A, Int]

object Test {
  def doesNotCompile[A[_]]: (({ type λ[α] = GADTK[A, α] })#λ ~> ({ type λ[α] = GADTK[A, α] })#λ) =
    new (({ type λ[α] = GADTK[A, α] })#λ ~> ({ type λ[α] = GADTK[A, α] })#λ) {
      def apply[I](v: GADTK[A, I]): GADTK[A, I] = v match {
        case MemberK(i) => MemberK(i)
      }
    }
}
