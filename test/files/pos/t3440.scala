object test {
  abstract class SampleFormat1 {
    def readerFactory: Any
  }

  case object Int8 extends SampleFormat1 {
    def readerFactory = sys.error("")
  }
  case object Int16 extends SampleFormat1 {
    def readerFactory = sys.error("")
  }

  (new {}: Any) match {
   case 8   => Int8
   case 16  => Int16
   case _   => sys.error("")
  }
}
