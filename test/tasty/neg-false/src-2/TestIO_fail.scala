package tastytest

/** Suspended as can't parse by name params yet
 */
object TestIO {

  def putStrLn(msg: String): IO[Unit] = IO.effect(println(msg))
  def readNextInt: IO[Int] = IO.effect(io.StdIn.readInt())

  val dblProgram = for {
    in  <- readNextInt
    dbl <- IO.succeed(in * 2)
    _   <- putStrLn(dbl.toString)
  } yield ()

}
