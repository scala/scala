package p {
  final class S extends Shape

  // Java sources are compiled in the same run, `knownDirectSubclasses` needs to complete the classes named in `permits`
  object T { def run(): Unit = println(Macros.subs[Shape]) }
}

object Test {
  def main(args: Array[String]): Unit = p.T.run()
}
