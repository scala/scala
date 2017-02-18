object Test {
  def main(args: Array[String]): Unit = {
    try {
      Int.box(???) // crashed the compiler: java.util.NoSuchElementException: key not found: Lscala/runtime/Nothing$;
      sys.error("no exception")
    } catch { 
      case _: NotImplementedError =>
        // oka
      case e: Throwable => 
        sys.error("wrong exception: " + e)
    }
  }
}
