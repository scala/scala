/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import compat.Math.log
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object MainTokenMetric {

  private var reporter: ConsoleReporter = _

  def tokenMetric(compiler: Global, fnames: List[String]): unit = {
    import compiler.CompilationUnit
    import compiler.syntaxAnalyzer.Scanner
    import ast.parser.Tokens.EOF
    var totale = 0
    for (val source <- fnames) {
      val s = new Scanner(new CompilationUnit(compiler.getSourceFile(source)))
      var i = 0
      while(s.token != EOF) {
        i = i + 1
        s.nextToken()
      }
      var j = 0 ; while(j + log(i) / log(10) < 7) {
        j = j+1
        Console.print(' ')
      }
      Console.print(i.toString())
      Console.print(" ")
      Console.println(source.toString())
      totale = totale + i
    }
    Console.println(totale.toString()+" total")
  }

  def process(args: Array[String]): unit = {
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

  def main(args: Array[String]): unit = {
    process(args)
    exit(if (reporter.hasErrors) 1 else 0)
  }

}
