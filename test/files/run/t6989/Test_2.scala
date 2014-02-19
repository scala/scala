import scala.reflect.runtime.universe._

// Originally composed to accommodate pull request feedback, this test has
// uncovered a handful of bugs in FromJavaClassCompleter, namely:
// * SI-7071 non-public ctors get lost
// * SI-7072 inner classes are read incorrectly

// I'm leaving the incorrect results of FromJavaClassCompleters in the check
// file, so that we get notified when something changes there.

package object foo {
  def testAll(): Unit = {
    test(typeOf[foo.PackagePrivateJavaClass].typeSymbol)
    test(typeOf[foo.PackagePrivateJavaClass].typeSymbol.companion)
    test(typeOf[foo.JavaClass_1].typeSymbol)
    test(typeOf[foo.JavaClass_1].typeSymbol.companion)
  }

  def test(sym: Symbol): Unit = {
    printSymbolDetails(sym)
    if (sym.isClass || sym.isModule) {
      sym.info.decls.toList.sortBy(_.name.toString) foreach test
    }
  }

  def printSymbolDetails(sym: Symbol): Unit = {
    def stableSignature(sym: Symbol) = sym.info match {
      case ClassInfoType(_, _, _) => "ClassInfoType(...)"
      case tpe => tpe.toString
    }
    println("============")
    println(s"sym = $sym, signature = ${stableSignature(sym)}, owner = ${sym.owner}")
    println(s"isPrivate = ${sym.isPrivate}")
    println(s"isProtected = ${sym.isProtected}")
    println(s"isPublic = ${sym.isPublic}")
    println(s"privateWithin = ${sym.privateWithin}")
  }
}

object Test extends App {
  foo.testAll()
}