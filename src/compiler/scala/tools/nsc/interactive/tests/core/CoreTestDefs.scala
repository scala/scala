package scala.tools.nsc
package interactive
package tests.core

import scala.tools.nsc.util.Position
import scala.tools.nsc.interactive.tests.core._

/** Set of core test definitions that are executed for each test run. */
private[tests] trait CoreTestDefs
	extends PresentationCompilerRequestsWorkingMode {

  import scala.tools.nsc.interactive.Global

  /** Ask the presentation compiler for completion at all locations
   * (in all sources) where the defined `marker` is found. */
  class CompletionAction(override val compiler: Global)
    extends PresentationCompilerTestDef
    with AskCompletionAt {

    object MemberPrinter {
      def apply(member: compiler.Member): String =
        "`" + (member.sym.toString() + member.tpe.toString()).trim() + "`"
    }

    protected val marker = CompletionMarker

    override def runTest() {
      askAllSources(marker) { pos =>
        askCompletionAt(pos)
      } { (pos, members) =>
        withResponseDelimiter {
          reporter.println("[response] aksTypeCompletion at " + format(pos))
          // we skip getClass because it changed signature between 1.5 and 1.6, so there is no
          // universal check file that we can provide for this to work
          reporter.println("retrieved %d members".format(members.size))
          compiler ask { () =>
            val filtered = members.filterNot(member => member.sym.name.toString == "getClass" || member.sym.isConstructor)
            reporter.println(filtered.map(MemberPrinter(_)).sortBy(_.toString()).mkString("\n"))
          }
        }
      }
    }
  }

  /** Ask the presentation compiler for type info at all locations
   * (in all sources) where the defined `marker` is found. */
  class TypeAction(override val compiler: Global)
    extends PresentationCompilerTestDef
    with AskTypeAt {

    protected val marker = TypeMarker

    override def runTest() {
      askAllSources(marker) { pos =>
        askTypeAt(pos)
      } { (pos, tree) =>
        withResponseDelimiter {
          reporter.println("[response] askTypeAt at " + format(pos))
          compiler.ask(() => reporter.println(tree))
        }
      }
    }
  }

  /** Ask the presentation compiler for hyperlink at all locations
   * (in all sources) where the defined `marker` is found. */
  class HyperlinkAction(override val compiler: Global)
    extends PresentationCompilerTestDef
    with AskTypeAt
    with AskCompletionAt {

    protected val marker = HyperlinkMarker

    override def runTest() {
      askAllSources(marker) { pos =>
        askTypeAt(pos)(NullReporter)
      } { (pos, tree) =>
        if(tree.symbol == compiler.NoSymbol) {
          reporter.println("\nNo symbol is associated with tree: "+tree)
        }
        else {
          reporter.println("\naskHyperlinkPos for `" + tree.symbol.name + "` at " + format(pos) + " " + pos.source.file.name)
          val r = new Response[Position]
          // `tree.symbol.sourceFile` was discovered to be null when testing -Yvirtpatmat on the akka presentation test, where a position had shifted to point to `Int`
          // askHyperlinkPos for `Int` at (73,19) pi.scala --> class Int in package scala has null sourceFile!
          val treePath = if (tree.symbol.sourceFile ne null) tree.symbol.sourceFile.path else null
          val treeName = if (tree.symbol.sourceFile ne null) tree.symbol.sourceFile.name else null
          val sourceFile = sourceFiles.find(_.path == treePath) match {
            case Some(source) =>
              compiler.askLinkPos(tree.symbol, source, r)
              r.get match {
                case Left(pos) =>
                  withResponseDelimiter {
                    reporter.println("[response] found askHyperlinkPos for `" + tree.symbol.name + "` at " + format(pos) + " " + tree.symbol.sourceFile.name)
                  }
                case Right(ex) =>
                  ex.printStackTrace()
              }
            case None =>
              reporter.println("[error] could not locate sourcefile `" + treeName + "`." +
                "Hint: Does the looked up definition come form a binary?")
          }
        }
      }
    }
  }
}