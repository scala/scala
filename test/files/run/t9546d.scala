class X {
  def test: Any = {
    object Opt {
      def mkOpt = Opt("")
    }
    case class Opt[A] private[X](val get: A)
    Opt.mkOpt
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    new X().test
  }
}

