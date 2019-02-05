import scala.language.higherKinds
object t4043 {
  trait GC[K[_ <: H0], H0]

  trait PA[H1] {
    type Apply[A <: H1] = Any
  }

  type a = GC[PA[Int]#Apply, Int]
}
