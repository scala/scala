package scala.tools.nsc
package backend.jvm

import scala.collection.mutable
import scala.tools.asm.tree.ClassNode

abstract class CodeGen[G <: Global](val global: G) extends PerRunInit {
  val bTypes: BTypesFromSymbols[global.type]

  import global._
  import bTypes._

  private val caseInsensitively = perRunCaches.newMap[String, Symbol]()

  // TODO: do we really need a new instance per run? Is there state that depends on the compiler frontend (symbols, types, settings)?
  private[this] lazy val mirrorCodeGen: LazyVar[CodeGenImpl.JMirrorBuilder] = perRunLazy(this)(new CodeGenImpl.JMirrorBuilder())

  private[this] lazy val beanInfoCodeGen: LazyVar[CodeGenImpl.JBeanInfoBuilder] = perRunLazy(this)(new CodeGenImpl.JBeanInfoBuilder())

  def genUnit(unit: CompilationUnit): List[GeneratedClass] = {
    val res = mutable.ListBuffer.empty[GeneratedClass]

    def genClassDef(cd: ClassDef): Unit = try {
      val sym = cd.symbol
      val sourceFile = unit.source.file
      res += GeneratedClass(genClass(cd, unit), sourceFile, isArtifact = false)
      if (bTypes.isTopLevelModuleClass(sym)) {
        if (sym.companionClass == NoSymbol)
          res += GeneratedClass(genMirrorClass(sym, unit), sourceFile, isArtifact = true)
        else
          log(s"No mirror class for module with linked class: ${sym.fullName}")
      }
      if (sym hasAnnotation coreBTypes.BeanInfoAttr)
        res += GeneratedClass(genBeanInfoClass(cd, unit), sourceFile, isArtifact = true)
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
        error(s"Error while emitting ${unit.source}\n${ex.getMessage}")
    }

    def genClassDefs(tree: Tree): Unit = tree match {
      case EmptyTree => ()
      case PackageDef(_, stats) => stats foreach genClassDefs
      case cd: ClassDef => genClassDef(cd)
    }

    genClassDefs(unit.body)
    res.toList
  }

  def genClass(cd: ClassDef, unit: CompilationUnit): ClassNode = {
    warnCaseInsensitiveOverwrite(cd)
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

  private def warnCaseInsensitiveOverwrite(cd: ClassDef): Unit = {
    val sym = cd.symbol
    // GenASM checks this before classfiles are emitted, https://github.com/scala/scala/commit/e4d1d930693ac75d8eb64c2c3c69f2fc22bec739
    val lowercaseJavaClassName = sym.javaClassName.toLowerCase
    caseInsensitively.get(lowercaseJavaClassName) match {
      case None =>
        caseInsensitively.put(lowercaseJavaClassName, sym)
      case Some(dupClassSym) =>
        reporter.warning(
          sym.pos,
          s"Class ${sym.javaClassName} differs only in case from ${dupClassSym.javaClassName}. " +
            "Such classes will overwrite one another on case-insensitive filesystems."
        )
    }
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
