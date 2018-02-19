package scala.tools.nsc
package backend.jvm

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.Statistics
import scala.tools.asm.tree.ClassNode

abstract class CodeGen[G <: Global](val global: G) extends PerRunInit {
  val bTypes: BTypesFromSymbols[global.type]

  import global._
  import bTypes._
  import genBCode.generatedClassHandler

  // TODO: do we really need a new instance per run? Is there state that depends on the compiler frontend (symbols, types, settings)?
  private[this] lazy val mirrorCodeGen: LazyVar[CodeGenImpl.JMirrorBuilder] = perRunLazy(this)(new CodeGenImpl.JMirrorBuilder())

  private[this] lazy val beanInfoCodeGen: LazyVar[CodeGenImpl.JBeanInfoBuilder] = perRunLazy(this)(new CodeGenImpl.JBeanInfoBuilder())

  /**
   * Generate ASM ClassNodes for classes found in a compilation unit. The resulting classes are
   * passed to the `genBCode.generatedClassHandler`.
   */
  def genUnit(unit: CompilationUnit): Unit = {
    val generatedClasses = ListBuffer.empty[GeneratedClass]

    def genClassDef(cd: ClassDef): Unit = try {
      val sym = cd.symbol
      val position = sym.pos
      val fullSymbolName = sym.javaClassName
      val mainClassNode = genClass(cd, unit)
      generatedClasses += GeneratedClass(mainClassNode, fullSymbolName, position, isArtifact = false)
      if (bTypes.isTopLevelModuleClass(sym)) {
        if (sym.companionClass == NoSymbol) {
          val mirrorClassNode = genMirrorClass(sym, unit)
          generatedClasses += GeneratedClass(mirrorClassNode, fullSymbolName, position, isArtifact = true)
        }
        else
          log(s"No mirror class for module with linked class: ${sym.fullName}")
      }
      if (sym hasAnnotation coreBTypes.BeanInfoAttr) {
        val beanClassNode = genBeanInfoClass(cd, unit)
        generatedClasses += GeneratedClass(beanClassNode, fullSymbolName, position, isArtifact = true)
      }
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
        error(s"Error while emitting ${unit.source}\n${ex.getMessage}")
    }

    def genClassDefs(tree: Tree): Unit = tree match {
      case EmptyTree => ()
      case PackageDef(_, stats) => stats foreach genClassDefs
      case cd: ClassDef => frontendAccess.frontendSynch(genClassDef(cd))
    }

    statistics.timed(statistics.bcodeGenStat) {
      genClassDefs(unit.body)
    }

    generatedClassHandler.process(GeneratedCompilationUnit(unit.source.file, generatedClasses.toList))
  }

  def genClass(cd: ClassDef, unit: CompilationUnit): ClassNode = {
    addSbtIClassShim(cd)

    // TODO: do we need a new builder for each class? could we use one per run? or one per Global compiler instance?
    val b = new CodeGenImpl.SyncAndTryBuilder(unit)
    b.genPlainClass(cd)
    b.cnode
  }

  def genMirrorClass(classSym: Symbol, unit: CompilationUnit): ClassNode = {
    mirrorCodeGen.get.genMirrorClass(classSym, unit)
  }

  def genBeanInfoClass(cd: ClassDef, unit: CompilationUnit): ClassNode = {
    val sym = cd.symbol
    beanInfoCodeGen.get.genBeanInfoClass(sym, unit, CodeGenImpl.fieldSymbols(sym), CodeGenImpl.methodSymbols(cd))
  }

  private def addSbtIClassShim(cd: ClassDef): Unit = {
    // shim for SBT, see https://github.com/sbt/sbt/issues/2076
    // TODO put this closer to classfile writing once we have closure elimination
    // TODO create a nicer public API to find out the correspondence between sourcefile and ultimate classfiles
    currentUnit.icode += new icodes.IClass(cd.symbol)
  }

  object CodeGenImpl extends {
    val global: CodeGen.this.global.type = CodeGen.this.global
    val bTypes: CodeGen.this.bTypes.type = CodeGen.this.bTypes
  } with BCodeSyncAndTry
}
