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

    def monoImpl(using QuoteContext) = '{1}
    def polyImpl[T: Type](using QuoteContext) = Expr(summon[Type[T]].show)

    def posImpl(using qctx: QuoteContext): Expr[Position] = {
      import qctx.reflect.given
      val name = qctx.reflect.rootPosition.sourceFile.jpath.getFileName.toString
      val line = qctx.reflect.rootPosition.startLine + 1
      '{ Position(${Expr(name)}, ${Expr(line)}) }
    }

    def constIntImpl[T: Type](x: Expr[T])(using QuoteContext): Expr[Int] = '{1}

  }

}
