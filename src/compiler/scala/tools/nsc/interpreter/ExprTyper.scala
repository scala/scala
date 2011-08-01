/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import util.BatchSourceFile
import scala.tools.nsc.ast.parser.Tokens.EOF

trait ExprTyper {
  val repl: IMain

  import repl._
  import replTokens.{ Tokenizer }
  import global.{ reporter => _, Import => _, _ }
  import syntaxAnalyzer.{ UnitParser, UnitScanner, token2name }
  import naming.freshInternalVarName

  object codeParser extends { val global: repl.global.type = repl.global } with CodeHandlers[Tree] {
    def applyRule[T](code: String, rule: UnitParser => T): T = {
      reporter.reset()
      val unit    = new CompilationUnit(new BatchSourceFile("<console>", code))
      val scanner = new UnitParser(unit)
      val result  = rule(scanner)

      if (!reporter.hasErrors)
        scanner.accept(EOF)

      result
    }
    def tokens(code: String) = {
      reporter.reset()
      val unit = new CompilationUnit(new BatchSourceFile("<tokens>", code))
      val in   = new UnitScanner(unit)
      in.init()

      new Tokenizer(in) tokenIterator
    }

    def decl(code: String)  = CodeHandlers.fail("todo")
    def defn(code: String)  = CodeHandlers.fail("todo")
    def expr(code: String)  = applyRule(code, _.expr())
    def impt(code: String)  = applyRule(code, _.importExpr())
    def impts(code: String) = applyRule(code, _.importClause())
    def stmts(code: String) = applyRule(code, _.templateStatSeq(false)._2)
    def stmt(code: String)  = stmts(code) match {
      case List(t)  => t
      case xs       => CodeHandlers.fail("Not a single statement: " + xs.mkString(", "))
    }
  }

  /** Parse a line into a sequence of trees. Returns None if the input is incomplete. */
  def parse(line: String): Option[List[Tree]] = {
    var isIncomplete = false
    reporter.withIncompleteHandler((_, _) => isIncomplete = true) {
      val trees = codeParser.stmts(line)
      if (reporter.hasErrors) Some(Nil)
      else if (isIncomplete) None
      else Some(trees)
    }
  }
  def tokens(line: String) = codeParser.tokens(line)

  // TODO: integrate these into a CodeHandler[Type].

  // XXX literals.
  // 1) Identifiers defined in the repl.
  // 2) A path loadable via getModule.
  // 3) Try interpreting it as an expression.
  private var typeOfExpressionDepth = 0
  def typeOfExpression(expr: String, silent: Boolean = true): Option[Type] = {
    repltrace("typeOfExpression(" + expr + ")")
    if (typeOfExpressionDepth > 2) {
      repldbg("Terminating typeOfExpression recursion for expression: " + expr)
      return None
    }

    def asQualifiedImport = {
      val name = expr.takeWhile(_ != '.')
      importedTermNamed(name) flatMap { sym =>
        typeOfExpression(sym.fullName + expr.drop(name.length), true)
      }
    }
    def asModule = safeModule(expr) map (_.tpe)
    def asExpr = {
      val lhs = freshInternalVarName()
      val line = "lazy val " + lhs + " =\n" + expr

      interpret(line, true) match {
        case IR.Success => typeOfExpression(lhs, true)
        case _          => None
      }
    }
    def evaluate() = {
      typeOfExpressionDepth += 1
      try typeOfTerm(expr) orElse asModule orElse asExpr orElse asQualifiedImport
      finally typeOfExpressionDepth -= 1
    }

    // Don't presently have a good way to suppress undesirable success output
    // while letting errors through, so it is first trying it silently: if there
    // is an error, and errors are desired, then it re-evaluates non-silently
    // to induce the error message.
    beSilentDuring(evaluate()) orElse beSilentDuring(typeOfDeclaration(expr)) orElse {
      if (!silent)
        evaluate()

      None
    }
  }
  // Since people will be giving us ":t def foo = 5" even though that is not an
  // expression, we have a means of typing declarations too.
  private def typeOfDeclaration(code: String): Option[Type] = {
    repltrace("typeOfDeclaration(" + code + ")")
    val obname = freshInternalVarName()

    interpret("object " + obname + " {\n" + code + "\n}\n", true) match {
      case IR.Success =>
        val sym = symbolOfTerm(obname)
        if (sym == NoSymbol) None else {
          // TODO: bitmap$n is not marked synthetic.
          val decls = sym.tpe.decls.toList filterNot (x => x.isConstructor || x.isPrivate || (x.name.toString contains "$"))
          repltrace("decls: " + decls)
          decls.lastOption map (decl => typeCleanser(sym, decl.name))
        }
      case _          =>
        None
    }
  }
  // def compileAndTypeExpr(expr: String): Option[Typer] = {
  //   class TyperRun extends Run {
  //     override def stopPhase(name: String) = name == "superaccessors"
  //   }
  // }
}
