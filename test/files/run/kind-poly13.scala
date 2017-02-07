import scala.language.higherKinds

object Test extends App {

  trait Bar[A[_]]
  trait Foo {
    type L[_ <: AnyKind]
  }
  type Aux[L0[_ <: AnyKind]] = Foo { type L[a <: AnyKind] = L0[a] }

  val l: Aux[Bar] = new Foo { type L[a[_]] = Bar[a] }


  trait Foo2 {
    type L <: AnyKind
  }
  type Aux2[L0 <: AnyKind] = Foo2 { type L = L0 }
  val l2: Aux2[List] = new Foo2 { type L[a] = List[a] }

  trait Foo3[M <: AnyKind] {
    type L <: AnyKind
  }
  type Aux3[M <: AnyKind, L0 <: AnyKind] = Foo3[M] { type L = L0 }

  val l3: Aux3[List, Int] = new Foo3[List] { type L = Int }


  trait ~>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

}



