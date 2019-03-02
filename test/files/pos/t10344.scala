object t10344 {
  def unwrap[F[_]](f: F[Unit] => Unit): Unit = ()
  val f: (=> Unit) => Unit = { _ => () }
  unwrap(f)
}
