/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package jvm

import scala.reflect.internal.util.Statistics
import scala.tools.asm
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.io.AbstractFile

abstract class GenBCode extends SubComponent {
  import global._

  val bTypes = new BTypesFromSymbols[global.type](global)
  val codeGen = new CodeGen[global.type](global) {
    val bTypes: GenBCode.this.bTypes.type = GenBCode.this.bTypes
  }

  val postProcessor = new PostProcessor[bTypes.type](bTypes)

  import codeGen.CodeGenImpl._
  import bTypes._

  // TODO: move to a context without Global
  private var bytecodeWriter: BytecodeWriter = null

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

      postProcessor.postProcessAndSendToDisk(Writer)
      bytecodeWriter.close()

      Statistics.stopTimer(BackendStats.bcodeTimer, bcodeStart)
    }

    private def initialize(): Unit = {
      val initStart = Statistics.startTimer(BackendStats.bcodeInitTimer)
      scalaPrimitives.init()
      bTypes.initializeCoreBTypes()
      bTypes.javaDefinedClasses.clear() // TODO: necessary? it's a per-run cache.
      bTypes.javaDefinedClasses ++= currentRun.symSource collect {
        case (sym, _) if sym.isJavaDefined => sym.javaBinaryNameString
      }
      codeGen.initialize()
      // initBytecodeWriter invokes fullName, thus we have to run it before the typer-dependent thread is activated.
      bytecodeWriter = initBytecodeWriter(cleanup.getEntryPoints)
      Statistics.stopTimer(BackendStats.bcodeInitTimer, initStart)
    }

    // TODO: move this to a context that doesn't have a Global in scope, requires moving ClassWriter
    object Writer extends ClassWriterForPostProcessor {
      val needOutFile = bytecodeWriter.isInstanceOf[ClassBytecodeWriter]
      def write(bytes: Array[Byte], className: InternalName, sourceFile: AbstractFile): Unit = {
        val writeStart = Statistics.startTimer(BackendStats.bcodeWriteTimer)
        try {
          // outFile is `null` if no file is written to disk (but, for example, to a jar)
          val outFile = if (needOutFile) {
            val outFolder = settings.outputDirs.outputDirFor(sourceFile)
            codeGen.CodeGenImpl.getFile(outFolder, className, ".class")
          } else null
          bytecodeWriter.writeClass(className, className, bytes, outFile)
        } catch {
          case e: FileConflictException =>
            backendReporting.error(NoPosition, s"error writing $className: ${e.getMessage}")
          case e: java.nio.file.FileSystemException =>
            if (settings.debug)
              e.printStackTrace()
            backendReporting.error(NoPosition, s"error writing $className: ${e.getClass.getName} ${e.getMessage}")
        }
        Statistics.stopTimer(BackendStats.bcodeWriteTimer, writeStart)
      }
    }
  }
}

object GenBCode {
  def mkFlags(args: Int*) = args.foldLeft(0)(_ | _)

  final val PublicStatic      = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC
  final val PublicStaticFinal = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_FINAL

  val CLASS_CONSTRUCTOR_NAME    = "<clinit>"
  val INSTANCE_CONSTRUCTOR_NAME = "<init>"
}
