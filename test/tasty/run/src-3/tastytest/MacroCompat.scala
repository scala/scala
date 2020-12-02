package tastytest

import scala.language.experimental.macros

object MacroCompat {

  implicit def pos: Position = macro Position.posImpl
  implicit inline def pos: Position = ${ Macros3.posImpl }

  def constInt[T](x: T): Int = macro Macros.constIntImpl[T]
  inline def constInt[T](x: T): Int = ${ Macros3.constIntImpl[T]('x) }

  object Bundles {
    def mono: Int = macro MacroImpl.mono
    inline def mono: Int = ${ Macros3.monoImpl }

    def poly[T]: String = macro MacroImpl.poly[T]
    inline def poly[T]: String = ${ Macros3.polyImpl[T] }
  }

  def testCase(test: => Any)(implicit pos: Position): (String, Position) =
    (String.valueOf(test), implicitly[Position])

  object Macros3 {
    import quoted._

    def monoImpl(using Quotes) = '{1}
    def polyImpl[T: Type](using quotes: Quotes) = Expr(quotes.reflect.TypeRepr.of[T].show)

    def posImpl(using quotes: Quotes): Expr[Position] = {
      import quotes.reflect.given
      val pos = quotes.reflect.Position.ofMacroExpansion
      val name = pos.sourceFile.jpath.getFileName.toString
      val line = pos.startLine + 1
      '{ Position(${Expr(name)}, ${Expr(line)}) }
    }

    def constIntImpl[T: Type](x: Expr[T])(using Quotes): Expr[Int] = '{1}

  }

}
