package tastytest

object Exports {

  class BitMap
  class InkJet

  class Printer {
    type PrinterType
    def print(bits: String): String = s"printed($bits)"
    def status: List[String] = "printing" :: Nil
  }

  class Scanner {
    def scan(): String = "scanned(foo)"
    def status: List[String] = "scanning" :: Nil
  }

  class Copier {
    private val printUnit = new Printer { type PrinterType = InkJet }
    private val scanUnit = new Scanner

    export scanUnit.scan
    export printUnit.{status => _, _}

    def status: List[String] = printUnit.status ++ scanUnit.status
  }
}
