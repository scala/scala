import scala.annotation.switch

object Test extends App {
  def noSwitch(ch: Char, eof: Boolean) = ch match {
    case 'a' if eof => println("a with oef") // then branch
  }

  def onlyThen(ch: Char, eof: Boolean) = ch match {
    case 'a' if eof => println("a with oef") // then branch
    case 'c' =>
  }

  def ifThenElse(ch: Char, eof: Boolean) = (ch: @switch) match {
    case 'a' if eof => println("a with oef") // then branch
    case 'a' if eof => println("a with oef2") // unreachable, but the analysis is not that sophisticated
    case 'a' => println("a") // else-branch
    case 'c' =>
  }

  def defaultUnguarded(ch: Char, eof: Boolean) = ch match {
    case ' ' if eof => println("spacey oef")
    case _ => println("default")
  }

  def defaults(ch: Char, eof: Boolean) = (ch: @switch) match {
    case _ if eof => println("def with oef") // then branch
    case _ if eof => println("def with oef2") // unreachable, but the analysis is not that sophisticated
    case _ => println("def") // else-branch
  }

  // test binders in collapsed cases (no need to run, it's "enough" to know it doesn't crash the compiler)
  def guard(x: Any): Boolean = true
  def testBinders =
    try { println("") } // work around SI-6015
    catch {
      case _ if guard(null) =>
      case x if guard(x) => throw x
    }

  // def unreachable(ch: Char) = (ch: @switch) match {
  //   case 'a' => println("b") // ok
  //   case 'a' => println("b") // unreachable
  //   case 'c' =>
  // }

  noSwitch('a', true)
  onlyThen('a', true)       // 'a with oef'
  ifThenElse('a', true)     // 'a with oef'
  ifThenElse('a', false)    // 'a'
  defaults('a', true)       // 'def with oef'
  defaults('a', false)      // 'def'

  // test that it jumps to default case, no match error
  defaultUnguarded(' ', false) // default
}