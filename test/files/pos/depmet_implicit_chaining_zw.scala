trait Zero
trait Succ[N]

trait ZipWith[N, S] {
  type T
  val x: T = sys.error("")
}

object ZipWith {
  implicit def ZeroZipWith[S] = new ZipWith[Zero, S] {
    type T = Stream[S]
  }

  implicit def SuccZipWith[N, S, R](implicit zWith : ZipWith[N, R]) = new ZipWith[Succ[N], S => R] {
    type T = Stream[S] => zWith.T // dependent types replace the associated types functionality
  }

  // can't use implicitly[ZipWith[Succ[Succ[Zero]], Int => String => Boolean]],
  // since that will chop of the {type T = ... } refinement in adapt (pt = ZipWith[Succ[Succ[Zero]], Int => String => Boolean])
  // this works
  // def zipWith(implicit zw: ZipWith[Succ[Succ[Zero]], Int => String => Boolean]): zw.T = zw.x
  // thus, I present ?: implicitly on steroids!
  def ?[T <: AnyRef](implicit w: T): w.type = w

  type _2 = Succ[Succ[Zero]]
  val zw = ?[ZipWith[_2, Int => String => Boolean]].x // : Stream[Int] => Stream[String] => Stream[Boolean]
  // val zw = implicitly[ZipWith[Succ[Succ[Zero]], Int => String => Boolean]{type T = Stream[Int] => Stream[String] => Stream[Boolean]}].x
}
