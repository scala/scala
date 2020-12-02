package tastytest

import scala.language.experimental.macros

object MacroCompat {

  implicit def pos: Position = macro Macros.posImpl // implemented in test/tasty/run/pre/tastytest/package.scala
  implicit inline def pos: Position = ${ Macros3.posImpl }

  def testCase(test: => Any)(using Position): String =
    s"${String.valueOf(test)} @@ ${summon[Position]}"

  object Macros3 {
    import quoted._

    def posImpl(using quotes: Quotes): Expr[Position] = {
      import quotes.reflect.given
      val pos = quotes.reflect.Position.ofMacroExpansion
      val name = pos.sourceFile.jpath.getFileName.toString
      val line = pos.startLine + 1
      '{ Position(${Expr(name)}, ${Expr(line)}) }
    }
  }

}
