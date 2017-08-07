/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package jvm

import scala.reflect.internal.util.Statistics
import scala.tools.asm

abstract class GenBCode extends SubComponent {
  import global._

  val bTypes = new BTypesFromSymbols[global.type](global)
  val codeGen = new CodeGen[global.type](global) {
    val bTypes: GenBCode.this.bTypes.type = GenBCode.this.bTypes
  }

  val postProcessor = new PostProcessor[bTypes.type](bTypes, () => cleanup.getEntryPoints)

  val phaseName = "jvm"

  override def newPhase(prev: Phase) = new BCodePhase(prev)

  class BCodePhase(prev: Phase) extends StdPhase(prev) {
    override def description = "Generate bytecode from ASTs using the ASM library"

    override val erasedTypes = true

    def apply(unit: CompilationUnit): Unit = codeGen.genUnit(unit)

    override def run(): Unit = {
      val bcodeStart = Statistics.startTimer(BackendStats.bcodeTimer)

      initialize()

      val genStart = Statistics.startTimer(BackendStats.bcodeGenStat)
      super.run() // invokes `apply` for each compilation unit
      Statistics.stopTimer(BackendStats.bcodeGenStat, genStart)

      postProcessor.postProcessAndSendToDisk()

      Statistics.stopTimer(BackendStats.bcodeTimer, bcodeStart)
    }

    /**
     * Several backend components have state that needs to be initialized in each run, because
     * it depends on frontend data that may change between runs: Symbols, Types, Settings.
     */
    private def initialize(): Unit = {
      val initStart = Statistics.startTimer(BackendStats.bcodeInitTimer)
      scalaPrimitives.init()
      bTypes.initialize()
      codeGen.initialize()
      postProcessor.initialize()
      Statistics.stopTimer(BackendStats.bcodeInitTimer, initStart)
    }
  }
}

object GenBCode {
  def mkFlags(args: Int*) = args.foldLeft(0)(_ | _)

  final val PublicStatic = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC
  final val PublicStaticFinal = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_FINAL

  val CLASS_CONSTRUCTOR_NAME = "<clinit>"
  val INSTANCE_CONSTRUCTOR_NAME = "<init>"
}
