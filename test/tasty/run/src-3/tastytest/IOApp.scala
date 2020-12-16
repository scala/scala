package tastytest

trait IOApp {
  protected val foo = 23

  def run(args: List[String]): Int

  final def main(args: Array[String]): Unit = {
    sys.exit(run(args.toList))
  }

}

object IOApp {
  trait Simple extends IOApp {
    def run: Unit

    final def run(args: List[String]): Int = {
      run
      0
    }
  }
}
