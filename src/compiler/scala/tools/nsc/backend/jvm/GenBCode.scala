/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala
package tools.nsc
package backend
package jvm

import scala.collection.{ mutable, immutable }
import scala.annotation.switch

import scala.tools.asm

/*
 *  Prepare in-memory representations of classfiles using the ASM Tree API, and serialize them to disk.
 *
 *  `BCodePhase.apply(CompilationUnit)` is invoked by some external force and that sets in motion:
 *    - visiting each ClassDef contained in that CompilationUnit
 *    - lowering the ClassDef into:
 *          (a) an optional mirror class,
 *          (b) a plain class, and
 *          (c) an optional bean class.
 *    - each of the ClassNodes above is lowered into a byte-array (ie into a classfile) and serialized.
 *
 *  Plain, mirror, and bean classes are built respectively by PlainClassBuilder, JMirrorBuilder, and JBeanInfoBuilder.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
abstract class GenBCode extends BCodeSyncAndTry {
  import global._

  val phaseName = "jvm"

  override def newPhase(prev: Phase) = new BCodePhase(prev)

  final class PlainClassBuilder(cunit: CompilationUnit) extends SyncAndTryBuilder(cunit)

  class BCodePhase(prev: Phase) extends StdPhase(prev) {

    override def name = phaseName
    override def description = "Generate bytecode from ASTs using the ASM library"
    override def erasedTypes = true

    private var bytecodeWriter  : BytecodeWriter   = null
    private var mirrorCodeGen   : JMirrorBuilder   = null
    private var beanInfoCodeGen : JBeanInfoBuilder = null

    private var needsOutFolder  = false // whether getOutFolder(claszSymbol) should be invoked for each claszSymbol

    val caseInsensitively = mutable.Map.empty[String, Symbol]

    /*
     *  Checks for duplicate internal names case-insensitively,
     *  builds ASM ClassNodes for mirror, plain, and bean classes.
     *
     */
    def visit(arrivalPos: Int, cd: ClassDef, cunit: CompilationUnit) {
      val claszSymbol = cd.symbol

      // GenASM checks this before classfiles are emitted, https://github.com/scala/scala/commit/e4d1d930693ac75d8eb64c2c3c69f2fc22bec739
      val lowercaseJavaClassName = claszSymbol.javaClassName.toLowerCase
      caseInsensitively.get(lowercaseJavaClassName) match {
        case None =>
          caseInsensitively.put(lowercaseJavaClassName, claszSymbol)
        case Some(dupClassSym) =>
          cunit.warning(
            claszSymbol.pos,
            s"Class ${claszSymbol.javaClassName} differs only in case from ${dupClassSym.javaClassName}. " +
            "Such classes will overwrite one another on case-insensitive filesystems."
          )
      }

      // -------------- mirror class, if needed --------------
      val mirrorC =
        if (isStaticModule(claszSymbol) && isTopLevelModule(claszSymbol)) {
          if (claszSymbol.companionClass == NoSymbol) {
            mirrorCodeGen.genMirrorClass(claszSymbol, cunit)
          } else {
            log(s"No mirror class for module with linked class: ${claszSymbol.fullName}");
            null
          }
        } else null

      // -------------- "plain" class --------------
      val pcb = new PlainClassBuilder(cunit)
      pcb.genPlainClass(cd)
      val outF = if (needsOutFolder) getOutFolder(claszSymbol, pcb.thisName, cunit) else null;
      val plainC = pcb.cnode

      // -------------- bean info class, if needed --------------
      val beanC =
        if (claszSymbol hasAnnotation BeanInfoAttr) {
          beanInfoCodeGen.genBeanInfoClass(
            claszSymbol, cunit,
            fieldSymbols(claszSymbol),
            methodSymbols(cd)
          )
        } else null

      // ----------- serialize classfiles to disk

      def getByteArray(cn: asm.tree.ClassNode): Array[Byte] = {
        val cw = new CClassWriter(extraProc)
        cn.accept(cw)
        cw.toByteArray
      }

      if (mirrorC != null) {
        sendToDisk(mirrorC.name, getByteArray(mirrorC), outF)
      }
      sendToDisk(plainC.name, getByteArray(plainC), outF)
      if (beanC != null) {
        sendToDisk(beanC.name, getByteArray(beanC), outF)
      }

    } // end of method visit()

    var arrivalPos = 0

    /*
     *  A run of the BCodePhase phase comprises:
     *
     *    (a) set-up steps (most notably supporting maps in `BCodeTypes`,
     *        but also "the" writer where class files in byte-array form go)
     *
     *    (b) building of ASM ClassNodes, their optimization and serialization.
     *
     *    (c) tear down (closing the classfile-writer and clearing maps)
     *
     */
    override def run() {

      arrivalPos = 0 // just in case
      scalaPrimitives.init
      initBCodeTypes()

      // initBytecodeWriter invokes fullName, thus we have to run it before the typer-dependent thread is activated.
      bytecodeWriter  = initBytecodeWriter(cleanup.getEntryPoints)
      mirrorCodeGen   = new JMirrorBuilder
      beanInfoCodeGen = new JBeanInfoBuilder

      needsOutFolder = bytecodeWriter.isInstanceOf[ClassBytecodeWriter]

      super.run()

      // closing output files.
      bytecodeWriter.close()

      caseInsensitively.clear()

      /* TODO Bytecode can be verified (now that all classfiles have been written to disk)
       *
       * (1) asm.util.CheckAdapter.verify()
       *       public static void verify(ClassReader cr, ClassLoader loader, boolean dump, PrintWriter pw)
       *     passing a custom ClassLoader to verify inter-dependent classes.
       *     Alternatively,
       *       - an offline-bytecode verifier could be used (e.g. Maxine brings one as separate tool).
       *       - -Xverify:all
       *
       * (2) if requested, check-java-signatures, over and beyond the syntactic checks in `getGenericSignature()`
       *
       */

      // clearing maps
      clearBCodeTypes()
    }

    def sendToDisk(jclassName: String, jclassBytes: Array[Byte], outFolder: _root_.scala.tools.nsc.io.AbstractFile) {
      try {
        val outFile =
          if (outFolder == null) null
          else getFileForClassfile(outFolder, jclassName, ".class")
        bytecodeWriter.writeClass(jclassName, jclassName, jclassBytes, outFile)
      }
      catch {
        case e: FileConflictException =>
          error(s"error writing $jclassName: ${e.getMessage}")
      }
    }

    override def apply(cunit: CompilationUnit): Unit = {

      def gen(tree: Tree) {
        tree match {
          case EmptyTree            => ()
          case PackageDef(_, stats) => stats foreach gen
          case cd: ClassDef         =>
            visit(arrivalPos, cd, cunit)
            arrivalPos += 1
        }
      }

      gen(cunit.body)
    }

  } // end of class BCodePhase

} // end of class GenBCode
