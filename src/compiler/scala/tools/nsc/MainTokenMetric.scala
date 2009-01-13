/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import scala.tools.nsc.reporters.ConsoleReporter

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object MainTokenMetric {

  private var reporter: ConsoleReporter = _

  private def tokenMetric(compiler: Global, fnames: List[String]) {
    import compiler.CompilationUnit
    import ast.parser.Tokens.EOF
    var totale = 0
    for (source <- fnames) {
      var i = 0
      /*
      import compiler.syntaxAnalyzer.UnitScanner
      val s = new UnitScanner(new CompilationUnit(compiler.getSourceFile(source)))
      while (s.token != EOF) {
        i += 1
        s.nextToken
      }*/
      var j = 0 ; while(j + Math.log(i) / Math.log(10) < 7) {
        j += 1
        Console.print(' ')
      }
      Console.print(i.toString())
      Console.print(" ")
      Console.println(source)
      totale += i
    }
    Console.println(totale.toString()+" total")
  }

  def process(args: Array[String]) {
    val settings = new Settings(error)
    reporter = new ConsoleReporter(settings)
    val command = new CompilerCommand(List.fromArray(args), settings, error, false)
    try {
      val compiler = new Global(command.settings, reporter)
      tokenMetric(compiler, command.files)
    } catch {
      case ex @ FatalError(msg) =>
        if (command.settings.debug.value)
          ex.printStackTrace();
      reporter.error(null, "fatal error: " + msg)
    }
  }

  def main(args: Array[String]) {
    process(args)
    exit(if (reporter.hasErrors) 1 else 0)
  }

}
