object Foo {
  implicit class MatCreator(val ctx: StringContext) extends AnyVal {
    def m(args: Any*): Unit = {
      ctx.checkLengths(args)
    }
    ???/*?*/
  }
}
