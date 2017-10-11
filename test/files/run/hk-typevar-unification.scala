import scala.language.higherKinds

trait Forall[F[_]] {
  def instantiate[A]: F[A]
}

object Forall {
  implicit class Ops[F[_]](f: Forall[F]) {
    def apply[A]: F[A] = f.instantiate[A]
  }
}

trait Forall2[F[_, _]] {
  def instantiate[A, B]: F[A, B]
}

object Forall2 {
  implicit class Ops[F[_, _]](f: Forall2[F]) {
    def apply[A, B]: F[A, B] = f.instantiate[A, B]
  }
}

trait FlatMap[F[_]] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object FlatMap {
  implicit val optionInstance: FlatMap[Option] = new FlatMap[Option] {
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]) = fa.flatMap(f)
  }
}

object Test extends App {

  // natural transformation
  type ~>[F[_], G[_]] = Forall[({ type L[A] = F[A] => G[A] })#L]

  // binatural transformation
  type ~~>[F[_, _], G[_, _]] = Forall2[({ type L[A, B] = F[A, B] => G[A, B] })#L]


  type RightAction[G[_], F[_, _]] = Forall2[({ type L[A, B] = (G[A], F[A, B]) => G[B] })#L]
  type  LeftAction[G[_], F[_, _]] = Forall2[({ type L[A, B] = (F[A, B], G[B]) => G[A] })#L]


  val headOpt = new (List ~> Option) {
    def instantiate[A]: List[A] => Option[A] = _.headOption
  }

  // tests that implicit Forall.Ops is found
  println(headOpt.apply(List(1, 2, 3)))
  println(headOpt[Int](List(1, 2, 3)))

  val someEntry = new (Map ~~> ({ type L[K, V] = Option[(K, V)] })#L) {
    def instantiate[K, V]: Map[K, V] => Option[(K, V)] = _.headOption
  }

  // tests that implicit Forall2.Ops is found
  println(someEntry.apply(Map(("hi", 5))))
  println(someEntry[String, Int](Map(("hi", 5))))

  def kleisliPostCompose[F[_], Z](implicit F: FlatMap[F]) =
    new RightAction[({ type L[A] = Z => F[A] })#L, ({ type L[A, B] = A => F[B] })#L] {
      def instantiate[A, B]: (Z => F[A], A => F[B]) => (Z => F[B]) = (f, g) => (z => F.flatMap(f(z))(g))
    }

  def kleisliPreCompose[F[_], C](implicit F: FlatMap[F]) =
    new LeftAction[({ type L[B] = B => F[C] })#L, ({ type L[A, B] = A => F[B] })#L] {
      def instantiate[A, B]: (A => F[B], B => F[C]) => (A => F[C]) = (f, g) => (a => F.flatMap(f(a))(g))
    }

  def parseInt(s: String): Option[Int] = Some(42)
  def toChar(i: Int): Option[Char] = Some('X')

  val ra = kleisliPostCompose[Option, String]
  val la = kleisliPreCompose[Option, Char]

  // tests that implicit Forall2.Ops is found
  println( ra.apply(parseInt(_), toChar(_)).apply("")  )
  println( ra[Int, Char](parseInt(_), toChar(_))("")   )
  println( la.apply(parseInt(_), toChar(_))("")        )
  println( la[String, Int](parseInt(_), toChar(_))("") )
}
