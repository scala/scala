/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package doc

import scala.util.control.ControlThrowable
import reporters.Reporter
import typechecker.Analyzer
import scala.reflect.internal.util.BatchSourceFile

trait ScaladocAnalyzer extends Analyzer {
  val global : Global // generally, a ScaladocGlobal
  import global._

  override def newTyper(context: Context): ScaladocTyper = new ScaladocTyper(context)

  class ScaladocTyper(context0: Context) extends Typer(context0) {
    private def unit = context.unit

    override def typedDocDef(docDef: DocDef, mode: Mode, pt: Type): Tree = {
      val sym = docDef.symbol

      if ((sym ne null) && (sym ne NoSymbol)) {
        val comment = docDef.comment
        docComments(sym) = comment
        comment.defineVariables(sym)
        val typer1 = newTyper(context.makeNewScope(docDef, context.owner))
        for (useCase <- comment.useCases) {
          typer1.silent(_ => typer1 defineUseCases useCase) match {
            case SilentTypeError(err) =>
              unit.warning(useCase.pos, err.errMsg)
            case _ =>
          }
          for (useCaseSym <- useCase.defined) {
            if (sym.name != useCaseSym.name)
              unit.warning(useCase.pos, "@usecase " + useCaseSym.name.decode + " does not match commented symbol: " + sym.name.decode)
          }
        }
      }

      super.typedDocDef(docDef, mode, pt)
    }

    def defineUseCases(useCase: UseCase): List[Symbol] = {
      def stringParser(str: String): syntaxAnalyzer.Parser = {
        val file = new BatchSourceFile(context.unit.source.file, str) {
          override def positionInUltimateSource(pos: Position) = {
            pos.withSource(context.unit.source, useCase.pos.start)
          }
        }
        val unit = new CompilationUnit(file)
        new syntaxAnalyzer.UnitParser(unit)
      }

      val trees = stringParser(useCase.body+";").nonLocalDefOrDcl
      val enclClass = context.enclClass.owner

      def defineAlias(name: Name) = (
        if (context.scope.lookup(name) == NoSymbol) {
          lookupVariable(name.toString.substring(1), enclClass) foreach { repl =>
            silent(_.typedTypeConstructor(stringParser(repl).typ())) map { tpt =>
              val alias = enclClass.newAliasType(name.toTypeName, useCase.pos)
              val tparams = cloneSymbolsAtOwner(tpt.tpe.typeSymbol.typeParams, alias)
              val newInfo = genPolyType(tparams, appliedType(tpt.tpe, tparams map (_.tpe)))
              alias setInfo newInfo
              context.scope.enter(alias)
            }
          }
        }
      )

      for (tree <- trees; t <- tree)
        t match {
          case Ident(name) if name startsWith '$' => defineAlias(name)
          case _ =>
        }

      useCase.aliases = context.scope.toList
      namer.enterSyms(trees)
      typedStats(trees, NoSymbol)
      useCase.defined = context.scope.toList filterNot (useCase.aliases contains _)

      if (settings.debug.value)
        useCase.defined foreach (sym => println("defined use cases: %s:%s".format(sym, sym.tpe)))

      useCase.defined
    }
  }
}

class ScaladocGlobal(settings: doc.Settings, reporter: Reporter) extends Global(settings, reporter) with interactive.RangePositions {
  override protected def computeInternalPhases() {
    phasesSet += syntaxAnalyzer
    phasesSet += analyzer.namerFactory
    phasesSet += analyzer.packageObjects
    phasesSet += analyzer.typerFactory
  }
  override def forScaladoc = true
  override lazy val analyzer = new {
    val global: ScaladocGlobal.this.type = ScaladocGlobal.this
  } with ScaladocAnalyzer
}
