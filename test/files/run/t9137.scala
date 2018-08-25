object Test {
  def main(args: Array[String]): Unit = {
    new Stream().ident1.next.next.next.next.next.next.next.next
    new Stream().ident2.next.next.next.next.next.next.next.next
  }

  class Stream() {
    val data = Array.ofDim[Byte](256 * 1024 * 1024)
    lazy val next = new Stream

    @noinline def id(s: Stream) = s

    @inline final def ident1: Stream = id(this)

    @inline final def ident2: Stream = {
      var alias: Stream = null
      if (data != null) alias = this
      alias
    }
  }
}