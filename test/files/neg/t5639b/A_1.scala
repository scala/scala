import Implicits._

class Baz

object Test {
  implicitly[Int]
}

object Implicits  {
  implicit val Baz: Int = 0
  // This implicit was being ignored by `isQualifyingImplicit`
  // if the classpath contained a class file for `class Baz`.
  // This is because the package scope contains a speculative
  // symbol for `object Baz` which is entered by `SymbolLoaders`
  // before looking inside the class file. (A Java originated
  // classfile results in the class/module symbol pair.)
}
