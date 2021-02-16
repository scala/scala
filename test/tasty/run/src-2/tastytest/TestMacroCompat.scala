package tastytest

import scala.language.experimental.macros

object TestMacroCompat extends Suite("TestMacroCompat") {
  import MacroCompat._
  import Bundles._

  test(assert(testCase(15) === ("15", Position("TestMacroCompat.scala", 9))))
  test(assert(constInt("") === 1))
  test(assert(mono === 1))
  test(assert(poly[String] === "String"))

}
