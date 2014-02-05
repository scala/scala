import language._

object ScalaZeee {
  trait HFold[M[_], U] {
    type Apply[E, A <: U] <: U
  }
  trait GenericCons[M[_], H, +T <: GenericList[M]] extends GenericList[M] {
    val tail: T
    override type Folded[N[X] >: M[X], U, F <: HFold[N, U]] = F#Apply[H, tail.Folded[N, U, F]]
  }
  val KNil: GenericList[Nothing] = ???
  sealed trait GenericList[+M[_]] {
     type Folded[N[X] >: M[X], U, F <: HFold[N, U]] <: U
  }
}
 
object TypelevelUsage {
  import ScalaZeee._
  type T = GenericCons[Some, String, KNil.type]
  val klist1: T = ???
  type T2 = klist1.Folded[Option, Int, HFold[Option, Int]]
  val count2: T2 = ???
   
  count2.ensuring(x => true).toChar // trigger an implicit search
}
