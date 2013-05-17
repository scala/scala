/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc

import scala.tools.nsc.reporters.ConsoleReporter

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object MainTokenMetric {

  private var reporter: ConsoleReporter = _

  def tokenMetric(compiler: Global, fnames: List[String]) {
    import compiler.CompilationUnit
    import compiler.syntaxAnalyzer.UnitScanner
    import ast.parser.Tokens.EOF
    var totale = 0
    for (source <- fnames) {
      val s = new UnitScanner(new CompilationUnit(compiler.getSourceFile(source)))
      s.nextToken()
      var i = 0
      while (s.token != EOF) {
        i += 1
        s.nextToken()
      }
      Console.println(i.toString + " " + source.toString())
      totale += i
    }
    Console.println(totale.toString()+" total")
  }

  def process(args: Array[String]) {
    val settings = new Settings(sys.error)
    reporter = new ConsoleReporter(settings)
    val command = new CompilerCommand(args.toList, settings)
    try {
      val compiler = new Global(command.settings, reporter)
      tokenMetric(compiler, command.files)
    } catch {
      case ex @ FatalError(msg) =>
        if (command.settings.debug)
          ex.printStackTrace()
        reporter.error(null, "fatal error: " + msg)
    }
  }

  def main(args: Array[String]) {
    process(args)
    sys.exit(if (reporter.hasErrors) 1 else 0)
  }

}
