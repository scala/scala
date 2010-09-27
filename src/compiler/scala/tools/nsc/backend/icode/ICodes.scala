/* NSC -- new scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
/* NSC -- new scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package icode

import java.io.PrintWriter

import scala.collection.mutable.HashMap
import scala.tools.nsc.symtab._
import analysis.{Liveness, ReachingDefinitions}
import scala.tools.nsc.symtab.classfile.ICodeReader

/** Glue together ICode parts.
 *
 *  @author Iulian Dragos
 */
abstract class ICodes extends AnyRef
                                 with Members
                                 with BasicBlocks
                                 with Opcodes
                                 with TypeStacks
                                 with TypeKinds
                                 with ExceptionHandlers
                                 with Primitives
                                 with Linearizers
                                 with Printers
                                 with Repository
{
  val global: Global

  /** The ICode representation of classes */
  var classes: HashMap[global.Symbol, IClass] = new HashMap()

  /** Debugging flag */
  var isCheckerDebug: Boolean = global.settings.checkDebug.value
  def checkerDebug(msg: String) = if (isCheckerDebug) println(msg)

  /** The ICode linearizer. */
  val linearizer: Linearizer =
    if (global.settings.Xlinearizer.value == "rpo")
      new ReversePostOrderLinearizer()
    else if (global.settings.Xlinearizer.value == "dfs")
      new DepthFirstLinerizer()
    else if (global.settings.Xlinearizer.value == "normal")
      new NormalLinearizer();
    else if (global.settings.Xlinearizer.value == "dump")
      new DumpLinearizer()
    else
      global.abort("Unknown linearizer: " + global.settings.Xlinearizer.value)

  /** Have to be careful because dump calls around, possibly
   *  re-entering methods which initiated the dump (like foreach
   *  in BasicBlocks) which leads to the icode output olympics.
   */
  private var alreadyDumping = false

  /** Print all classes and basic blocks. Used for debugging. */

  def dump {
    if (alreadyDumping) return
    else alreadyDumping = true

    val printer = new TextPrinter(new PrintWriter(Console.out, true),
                                  new DumpLinearizer)

    classes.values foreach printer.printClass
  }

  def checkValid(m: IMethod) {
    for (b <- m.code.blocks)
      if (!b.closed) {
        m.dump
        global.abort("Open block: " + b.flagsString)
      }
  }

  object liveness extends Liveness {
    val global: ICodes.this.global.type = ICodes.this.global
  }

  object reachingDefinitions extends ReachingDefinitions {
    val global: ICodes.this.global.type = ICodes.this.global
  }

  lazy val ObjectReference: TypeKind    = REFERENCE(global.definitions.ObjectClass)
  lazy val ThrowableReference: TypeKind = REFERENCE(global.definitions.ThrowableClass)

  object icodeReader extends ICodeReader {
    lazy val global: ICodes.this.global.type = ICodes.this.global
  }

  /** A phase which works on icode. */
  abstract class ICodePhase(prev: Phase) extends global.GlobalPhase(prev) {
    override def erasedTypes = true

    override def apply(unit: global.CompilationUnit) {
      unit.icode foreach { c => apply(c) }
    }

    def apply(cls: global.icodes.IClass): Unit
  }
}

