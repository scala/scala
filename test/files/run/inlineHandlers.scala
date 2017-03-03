object Test {
  @noinline def ham: String = throw null
  @inline def inner: String = try { ham } catch { case _: NullPointerException => "npe" }
  def foo = try inner catch { case e: Throwable => throw e }

  def main(args: Array[String]): Unit = assert(foo == "npe")
}
