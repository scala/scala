package tastytest

object TestExports extends Suite("TestExports") {
  val c = new Exports.Copier()

  def extract(scanned: String): String = scanned match {
    case s"scanned($inner)" => inner
    case x                  => throw new MatchError(x)
  }

  def makePrinter(copier: Exports.Copier)(f: copier.type => copier.PrinterType): copier.PrinterType =
    f(copier)

  test(assert(c.scan() === "scanned(foo)"))
  test(assert(c.print(extract(c.scan())) === "printed(foo)"))
  test(assert(c.status === "printing" :: "scanning" :: Nil))
}
