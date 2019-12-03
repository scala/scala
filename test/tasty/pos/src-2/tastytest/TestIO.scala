package tastytest

/** Not in run, as IO input is not yet supported in test suites
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
