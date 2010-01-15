



object Test {
  val closeable1: { def close(): Unit } = new scala.io.Source { val iter: Iterator[Char] = "".iterator }
  val closeable2: { def close(): Unit } = new java.io.Closeable { def close() = {} }
}
