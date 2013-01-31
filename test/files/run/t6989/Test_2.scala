import scala.reflect.runtime.universe._

package object foo {
  def test(sym: Symbol) = {
    println("============")
    println(sym)
    println(s"isPrivate = ${sym.isPrivate}")
    println(s"isProtected = ${sym.isProtected}")
    println(s"isPublic = ${sym.isPublic}")
    println(s"privateWithin = ${sym.privateWithin}")
  }

  def testAll() = {
    test(typeOf[foo.JavaClass_1].typeSymbol)
    test(typeOf[foo.JavaClass_1].declaration(newTermName("x")))
    test(typeOf[foo.JavaClass_1].declaration(newTermName("y")))
    test(typeOf[foo.JavaClass_1].declaration(newTermName("z")))
  }
}

object Test extends App {
  foo.testAll()
}