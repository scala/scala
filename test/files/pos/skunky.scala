//> using options -Werror -Wnonunit-statement

import scala.reflect.macros._

trait Encoder[A]

object StringContextOps {
  class StringOpsMacros(val c: whitebox.Context) {
    import c.universe._
    def sql_impl(argSeq: Tree*): Tree = {
      val EncoderType = typeOf[Encoder[_]]
      argSeq.head
    }
  }
}
