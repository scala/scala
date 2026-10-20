package p {
  final class S extends Shape

  // `knownDirectSubclasses` completes the classes named in `permits`, which are compiled in this run
  object T { def run(): Unit = println(Macros.subs[Shape]) }
}

object Test {
  def main(args: Array[String]): Unit = p.T.run()
}
