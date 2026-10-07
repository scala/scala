//> using options -deprecation -Werror -Xreporter:scala.tools.partest.nest.PlainReporter
//
// todo -Yvalidate-pos:_

object Scala2Module {
  val singleLine = "string\u0023"
  val singleLineRaw = raw"string\u0023" //Compiler warning: Unicode escapes in raw interpolations are deprecated; use literal characters instead
  val multiline = """string\u0023""" //Compile warning: Unicode escapes in triple quoted strings are deprecated; use the literal character instead
  val multilineRaw = raw"""string\u0023"""
}
