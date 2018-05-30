object Test {
  def u1: Unit = Unit
  val u2: Unit = Unit
  lazy val u3: Unit = {
    println(5)
    Unit
  }

  def not_println(s: String): Unit = {
    println(s)
    Unit
  }

  def runA(thunk: => Unit) = thunk
  runA(Unit)
  runA({ println(1 + 1); Unit })

  def runB(f: Unit => Unit) = f(())
  runB { _ => println(2 + 3); Unit }

  lazy val ou: Either[Unit, Unit] =
    if (util.Random.nextInt % 2 == 0) Right(Unit) else Left(Unit)

  def noWarnNecessary: Unit.type = Unit
}
