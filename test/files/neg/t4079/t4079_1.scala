trait Functor[F[_]] {
  def map[A,B](fa: F[A], f: A => B): F[B]
}

trait ComposeT[F[_],G[_]] {
  type Apply[A] = F[G[A]]
}

case class Compose[F[_],G[_]]() {
  def Functor(implicit f: Functor[F], g: Functor[G]): Functor[ComposeT[F,G]#Apply] =
    new Functor[ComposeT[F,G]#Apply] {
      def map[A,B](c: ComposeT[F,G]#Apply[A], h: A => B) =
        f.map(c, (x:G[A]) => g.map(x,h))
    }
}

object Cat {
  def compose[F[_],G[_]] = Compose[F,G]()
}

object Functors {
  implicit val List = new Functor[List] {
    def map[A,B](fa: List[A], f: A => B): List[B] = fa map f
  }
  implicit val Option = new Functor[Option] {
    def map[A,B](fa: Option[A], f: A => B): Option[B] = fa map f
  }
}

object Main {
  import Functors._
  val cf = Cat.compose[List,Option].Functor
}
