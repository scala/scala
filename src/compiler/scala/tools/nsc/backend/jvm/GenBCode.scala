/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package backend
package jvm

import scala.annotation.nowarn
import scala.tools.asm.Opcodes

/**
 * Some notes about the backend's state and its initialization and release.
 *
 * State that is used in a single run is allocated through `recordPerRunCache`, for example
 * `ByteCodeRepository.compilingClasses` or `CallGraph.callsites`. This state is cleared at the end
 * of each run.
 *
 * Some state needs to be re-initialized per run, for example `CoreBTypes` (computed from Symbols /
 * Types) or the `GeneratedClassHandler` (depends on the compiler settings). This state is
 * (re-) initialized in the `GenBCode.initialize` method. There two categories:
 *
 *   1. State that is stored in a `var` field and (re-) assigned in the `initialize` method, for
 *      example the `GeneratedClassHandler`
 *   2. State that uses the `PerRunInit` / `bTypes.perRunLazy` / `LazyVar` infrastructure, for
 *      example the types in `CoreBTypes`
 *
 * The reason to use the `LazyVar` infrastructure is to prevent eagerly computing all the state
 * even if it's never used in a run. It can also be used to work around initialization ordering
 * issues, just like ordinary lazy vals. For state that is known to be accessed, a `var` field is
 * just fine.
 *
 * Typical `LazyVar` use: `lazy val state: LazyVar[T] = perRunLazy(component)(initializer)`
 *   - The `initializer` expression is executed lazily
 *   - When the initializer actually runs, it synchronizes on the
 *     `PostProcessorFrontendAccess.frontendLock`
 *   - The `component.initialize` method causes the `LazyVar` to be re-initialized on the next `get`
 *   - The `state` is itself a `lazy val` to make sure the `component.initialize` method only
 *     clears those `LazyVar`s that were ever accessed
 *
 * TODO: convert some uses of `LazyVar` to ordinary `var`.
 */
abstract class GenBCode extends SubComponent {
  self =>
  import global._
  import statistics._

  val postProcessorFrontendAccess: PostProcessorFrontendAccess = new PostProcessorFrontendAccess.PostProcessorFrontendAccessImpl(global)

  val bTypes: BTypesFromSymbols[global.type] = new { val frontendAccess = postProcessorFrontendAccess } with BTypesFromSymbols[global.type](global)

  val codeGen: CodeGen[global.type] = new { val bTypes: self.bTypes.type = self.bTypes } with CodeGen[global.type](global)

  val postProcessor: PostProcessor { val bTypes: self.bTypes.type } = new { val bTypes: self.bTypes.type = self.bTypes } with PostProcessor

  @nowarn("cat=lint-inaccessible")
  var generatedClassHandler: GeneratedClassHandler = _

  val phaseName = "jvm"

  override def newPhase(prev: Phase) = new BCodePhase(prev)

  class BCodePhase(prev: Phase) extends StdPhase(prev) {
    override def description = "Generate bytecode from ASTs using the ASM library"

    def apply(unit: CompilationUnit): Unit = codeGen.genUnit(unit)

    override def run(): Unit = {
      statistics.timed(bcodeTimer) {
        try {
          initialize()
          super.run() // invokes `apply` for each compilation unit
          generatedClassHandler.complete()
          writeOtherFiles()
        } finally {
          this.close()
        }
      }
    }

    /** See comment in [[GenBCode]] */
    private def initialize(): Unit = {
      val initStart = statistics.startTimer(bcodeInitTimer)
      scalaPrimitives.init()
      bTypes.initialize()
      codeGen.initialize()
      postProcessorFrontendAccess.initialize()
      postProcessor.initialize(global)
      generatedClassHandler = GeneratedClassHandler(global)
      statistics.stopTimer(statistics.bcodeInitTimer, initStart)
    }
    def writeOtherFiles(): Unit = {
      global.plugins foreach {
        plugin =>
          plugin.writeAdditionalOutputs(postProcessor.classfileWriter)
      }
    }

    private def close(): Unit =
      List[AutoCloseable](
        postProcessor.classfileWriter,
        generatedClassHandler,
        bTypes.BTypeExporter,
      ).filter(_ ne null).foreach(_.close())
  }
}

object GenBCode {
  final val PublicStatic = Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC
  final val PublicStaticFinal = Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL
  final val PrivateStaticFinal = Opcodes.ACC_PRIVATE | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL

  val CLASS_CONSTRUCTOR_NAME = "<clinit>"
  val INSTANCE_CONSTRUCTOR_NAME = "<init>"
}
