/* NSC -- new Scala compiler
 * Copyright 2009-2013 Typesafe/Scala Solutions and LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc
package interactive
package tests

import core._
import scala.collection.mutable.ListBuffer

/** A base class for writing interactive compiler tests.
 *
 *  This class tries to cover common functionality needed when testing the presentation
 *  compiler: instantiation source files, reloading, creating positions, instantiating
 *  the presentation compiler, random stress testing.
 *
 *  By default, this class loads all scala and java classes found under `src/`, going
 *  recursively into subfolders. Loaded classes are found in `sourceFiles`. trait `TestResources`
 *  The presentation compiler is available through `compiler`.
 *
 *  It is easy to test member completion, type and hyperlinking at a given position. Source
 *  files are searched for `TextMarkers`. By default, the completion marker is `/*!*/`, the
 *  typedAt marker is `/*?*/` and the hyperlinking marker is `/*#*/`. Place these markers in
 *  your source files, and the test framework will automatically pick them up and test the
 *  corresponding actions. Sources are reloaded by `askReload(sourceFiles)` (blocking
 *  call). All ask operations are placed on the work queue without waiting for each one to
 *  complete before asking the next. After all asks, it waits for each response in turn and
 *  prints the result. The default timeout is 1 second per operation.
 *
 *  To define a custom operation you have to:
 *
 *  	(1) Define a new marker by extending `TestMarker`
 *  	(2) Provide an implementation for the operation you want to check by extending `PresentationCompilerTestDef`
 *  	(3) Add the class defined in (1) to the set of executed test actions by calling `++` on `InteractiveTest`.
 *
 *  Then you can simply use the new defined `marker` in your test sources and the testing
 *  framework will automatically pick it up.
 *
 *  @see   Check existing tests under test/files/presentation
 *
 *  @author Iulian Dragos
 *  @author Mirco Dotta
 */
abstract class InteractiveTest
  extends AskParse
  with AskShutdown
  with AskReload
  with AskLoadedTyped
  with PresentationCompilerInstance
  with CoreTestDefs
  with InteractiveTestSettings { self =>

  protected val runRandomTests = false

  /** Should askAllSources wait for each ask to finish before issuing the next? */
  override protected val synchronousRequests = true

  /** The core set of test actions that are executed during each test run are
   *  `CompletionAction`, `TypeAction` and `HyperlinkAction`.
   *  Override this member if you need to change the default set of executed test actions.
   */
  protected lazy val testActions: ListBuffer[PresentationCompilerTestDef] = {
    ListBuffer(new TypeCompletionAction(compiler), new ScopeCompletionAction(compiler), new TypeAction(compiler), new HyperlinkAction(compiler))
  }

  /** Add new presentation compiler actions to test. Presentation compiler's test
   *  need to extends trait `PresentationCompilerTestDef`.
   */
  protected def ++(tests: PresentationCompilerTestDef*) {
    testActions ++= tests
  }

  /** Test's entry point */
  def main(args: Array[String]) {
    try execute()
    finally askShutdown()
  }

  protected def execute(): Unit = {
    util.stringFromStream { ostream =>
      Console.withOut(ostream) {
        loadSources()
        runDefaultTests()
      }
    }.lines.map(normalize).foreach(println)
  }

  protected def normalize(s: String) = s

  /** Load all sources before executing the test. */
  protected def loadSources() {
    // ask the presentation compiler to track all sources. We do
    // not wait for the file to be entirely typed because we do want
    // to exercise the presentation compiler on scoped type requests.
    askReload(sourceFiles)
    // make sure all sources are parsed before running the test. This
    // is because test may depend on the sources having been parsed at
    // least once
    askParse(sourceFiles)
  }

  /** Run all defined `PresentationCompilerTestDef` */
  protected def runDefaultTests() {
    //TODO: integrate random tests!, i.e.: if (runRandomTests) randomTests(20, sourceFiles)
    testActions.foreach(_.runTest())
  }

  /** Perform n random tests with random changes. */
  /****
  private def randomTests(n: Int, files: Array[SourceFile]) {
    val tester = new Tester(n, files, settings) {
      override val compiler = self.compiler
      override val reporter = new reporters.StoreReporter
    }
    tester.run()
  }
  ****/
}
