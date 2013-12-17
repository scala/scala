package scala.tools.nsc
package interactive
package tests.core

import scala.reflect.internal.util.Position

/** Set of core test definitions that are executed for each test run. */
private[tests] trait CoreTestDefs
	extends PresentationCompilerRequestsWorkingMode {

  import scala.tools.nsc.interactive.Global

  /** Ask the presentation compiler for completion at all locations
   * (in all sources) where the defined `marker` is found. */
  class TypeCompletionAction(override val compiler: Global)
    extends PresentationCompilerTestDef
    with AskTypeCompletionAt {

    override def runTest() {
      askAllSources(TypeCompletionMarker) { pos =>
        askTypeCompletionAt(pos)
      } { (pos, members) =>
        withResponseDelimiter {
          reporter.println("[response] askTypeCompletion at " + format(pos))
          // we skip getClass because it changed signature between 1.5 and 1.6, so there is no
          // universal check file that we can provide for this to work
          reporter.println("retrieved %d members".format(members.size))
          compiler ask { () =>
            val filtered = members.filterNot(member => (member.sym.name string_== "getClass") || member.sym.isConstructor)
            reporter println (filtered.map(_.forceInfoString).sorted mkString "\n")
          }
        }
      }
    }
  }

  /** Ask the presentation compiler for completion at all locations
   * (in all sources) where the defined `marker` is found. */
  class ScopeCompletionAction(override val compiler: Global)
    extends PresentationCompilerTestDef
    with AskScopeCompletionAt {

    override def runTest() {
      askAllSources(ScopeCompletionMarker) { pos =>
        askScopeCompletionAt(pos)
      } { (pos, members) =>
        withResponseDelimiter {
          reporter.println("[response] askScopeCompletion at " + format(pos))
          try {
            // exclude members not from source (don't have position), for more focused and self contained tests.
            def eligible(sym: compiler.Symbol) = sym.pos != compiler.NoPosition
            val filtered = members.filter(member => eligible(member.sym))
            
            reporter.println("retrieved %d members".format(filtered.size))
            compiler ask { () =>
              reporter.println(filtered.map(_.forceInfoString).sorted mkString "\n")
            }
          } catch {
            case t: Throwable =>
              t.printStackTrace()
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

    override def runTest() {
      askAllSources(TypeMarker) { pos =>
        askTypeAt(pos)
      } { (pos, tree) =>
        withResponseDelimiter {
          reporter.println("[response] askTypeAt " + format(pos))
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
    with AskTypeCompletionAt {

    override def runTest() {
      askAllSources(HyperlinkMarker) { pos =>
        askTypeAt(pos)(NullReporter)
      } { (pos, tree) =>
        if(tree.symbol == compiler.NoSymbol || tree.symbol == null) {
          reporter.println("\nNo symbol is associated with tree: "+tree)
        }
        else {
          reporter.println("\naskHyperlinkPos for `" + tree.symbol.name + "` at " + format(pos) + " " + pos.source.file.name)
          val r = new Response[Position]
          // `tree.symbol.sourceFile` was discovered to be null when testing using virtpatmat on the akka presentation test, where a position had shifted to point to `Int`
          // askHyperlinkPos for `Int` at (73,19) pi.scala --> class Int in package scala has null sourceFile!
          val treePath = if (tree.symbol.sourceFile ne null) tree.symbol.sourceFile.path else null
          val treeName = if (tree.symbol.sourceFile ne null) tree.symbol.sourceFile.name else null

          sourceFiles.find(_.path == treePath) match {
            case Some(source) =>
              compiler.askLinkPos(tree.symbol, source, r)
              r.get match {
                case Left(pos) =>
                  val resolvedPos = if (tree.symbol.pos.isDefined) tree.symbol.pos else pos
                  withResponseDelimiter {
                    reporter.println("[response] found askHyperlinkPos for `" + tree.symbol.name + "` at " + format(resolvedPos) + " " + tree.symbol.sourceFile.name)
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
