import language.higherKinds

trait P  [N1, +E1[X <: N1]]
trait PIn[N2, +E2[X <: N2]] extends P[Int,Any]

trait EI extends PIn[Int, Nothing]
trait NI extends PIn[Int, Nothing]

object Test {
  val lub = if (true) ??? : EI else ??? : NI
  val pin: PIn[Int,Nothing] = lub
}
