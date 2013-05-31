class Bad[X, Y](val v: Int) extends AnyVal {
  @annotation.tailrec final def notTailPos[Z](a: Int)(b: String) {
    this.notTailPos[Z](a)(b)
    println("tail")
  }

  @annotation.tailrec final def differentTypeArgs {
    {(); new Bad[String, Unit](0)}.differentTypeArgs
  }
}
