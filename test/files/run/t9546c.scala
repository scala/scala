package foo {
  case class Opt[A] private[foo](val get: A)
  object Opt {
    def mkOpt = Opt("")
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    foo.Opt.mkOpt
  }
}

