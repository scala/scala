/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package jvm

import scala.tools.asm.Opcodes

abstract class GenBCode extends SubComponent {
  self =>
  import global._
  import statistics._

  val postProcessorFrontendAccess: PostProcessorFrontendAccess = new PostProcessorFrontendAccess.PostProcessorFrontendAccessImpl(global)

  val bTypes: BTypesFromSymbols[global.type] = new { val frontendAccess = postProcessorFrontendAccess } with BTypesFromSymbols[global.type](global)

  val codeGen: CodeGen[global.type] = new { val bTypes: self.bTypes.type = self.bTypes } with CodeGen[global.type](global)

  val postProcessor: PostProcessor { val bTypes: self.bTypes.type } = new { val bTypes: self.bTypes.type = self.bTypes } with PostProcessor(statistics)

  // re-initialized per run, as it depends on compiler settings that may change
  var generatedClassHandler: GeneratedClassHandler = _

  val phaseName = "jvm"

  override def newPhase(prev: Phase) = new BCodePhase(prev)

  class BCodePhase(prev: Phase) extends StdPhase(prev) {
    override def description = "Generate bytecode from ASTs using the ASM library"

    override val erasedTypes = true

    def apply(unit: CompilationUnit): Unit = codeGen.genUnit(unit)

    override def run(): Unit = {
      statistics.timed(bcodeTimer) {
        try {
          initialize()
          super.run() // invokes `apply` for each compilation unit
          generatedClassHandler.complete()
        } finally {
          // When writing to a jar, we need to close the jarWriter.
          generatedClassHandler.close()
        }
      }
    }

    /**
     * Several backend components have state that needs to be initialized in each run, because
     * it depends on frontend data that may change between runs: Symbols, Types, Settings.
     */
    private def initialize(): Unit = {
      val initStart = statistics.startTimer(bcodeInitTimer)
      scalaPrimitives.init()
      bTypes.initialize()
      codeGen.initialize()
      postProcessorFrontendAccess.initialize()
      postProcessor.initialize()
      generatedClassHandler = GeneratedClassHandler(global)
      statistics.stopTimer(statistics.bcodeInitTimer, initStart)
    }
  }
}

object GenBCode {
  final val PublicStatic = Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC
  final val PublicStaticFinal = Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL

  val CLASS_CONSTRUCTOR_NAME = "<clinit>"
  val INSTANCE_CONSTRUCTOR_NAME = "<init>"
}
