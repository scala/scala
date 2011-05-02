/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package icode

import java.io.PrintWriter
import scala.collection.mutable
import scala.tools.nsc.symtab._
import analysis.{ Liveness, ReachingDefinitions }
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
  import global.{ definitions, settings }

  /** The ICode representation of classes */
  val classes = new mutable.HashMap[global.Symbol, IClass]

  /** Debugging flag */
  def shouldCheckIcode = settings.check contains global.genicode.phaseName
  def checkerDebug(msg: String) = if (shouldCheckIcode && global.opt.debug) println(msg)

  /** The ICode linearizer. */
  val linearizer: Linearizer = settings.Xlinearizer.value match {
    case "rpo"    => new ReversePostOrderLinearizer()
    case "dfs"    => new DepthFirstLinerizer()
    case "normal" => new NormalLinearizer()
    case "dump"   => new DumpLinearizer()
    case x        => global.abort("Unknown linearizer: " + x)
  }

  /** Have to be careful because dump calls around, possibly
   *  re-entering methods which initiated the dump (like foreach
   *  in BasicBlocks) which leads to the icode output olympics.
   */
  private var alreadyDumping = false

  /** Print all classes and basic blocks. Used for debugging. */

  def dump() {
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
        global.abort("Open block: " + b + " " + b.flagsString)
      }
  }

  object liveness extends Liveness {
    val global: ICodes.this.global.type = ICodes.this.global
  }

  object reachingDefinitions extends ReachingDefinitions {
    val global: ICodes.this.global.type = ICodes.this.global
  }

  lazy val AnyRefReference: TypeKind    = REFERENCE(definitions.AnyRefClass)
  lazy val BoxedUnitReference: TypeKind = REFERENCE(definitions.BoxedUnitClass)
  lazy val NothingReference: TypeKind   = REFERENCE(definitions.NothingClass)
  lazy val NullReference: TypeKind      = REFERENCE(definitions.NullClass)
  lazy val ObjectReference: TypeKind    = REFERENCE(definitions.ObjectClass)
  lazy val StringReference: TypeKind    = REFERENCE(definitions.StringClass)
  lazy val ThrowableReference: TypeKind = REFERENCE(definitions.ThrowableClass)

  object icodeReader extends ICodeReader {
    lazy val global: ICodes.this.global.type = ICodes.this.global
  }

  /** A phase which works on icode. */
  abstract class ICodePhase(prev: Phase) extends global.GlobalPhase(prev) {
    override def erasedTypes = true
    override def apply(unit: global.CompilationUnit): Unit =
      unit.icode foreach apply

    def apply(cls: global.icodes.IClass): Unit
  }
}

