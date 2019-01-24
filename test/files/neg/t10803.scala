import scala.language.higherKinds

trait DistributiveCategory[F[_, _]] { self =>
  val sum: CocartesianMonoidalCategory[F] {
    type Unit = self.Zero
  }
}

object DistributiveCategory {
  trait SymmetricDistributiveCategory[F[_, _]]

  object instances {
    type Void
    type Dummy

    implicit val function1: SymmetricDistributiveCategory[Function1] {
      type Obj[A]
    } = new SymmetricDistributiveCategory[Function1] with CCC[Function1] { self =>
      type Zero
      val sum = new CocartesianMonoidalCategory[F] {}
    }
  }
}
