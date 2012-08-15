/* NSC -- new scala compiler
 * Copyright 2005-2012 LAMP/EPFL
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
  import global.{ log, definitions, settings, perRunCaches }

  /** The ICode representation of classes */
  val classes = perRunCaches.newMap[global.Symbol, IClass]()

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

  def newTextPrinter() =
    new TextPrinter(new PrintWriter(Console.out, true), new DumpLinearizer)

  /** Have to be careful because dump calls around, possibly
   *  re-entering methods which initiated the dump (like foreach
   *  in BasicBlocks) which leads to the icode output olympics.
   */
  private var alreadyDumping = false

  /** Print all classes and basic blocks. Used for debugging. */

  def dumpClassesAndAbort(msg: String): Nothing = {
    if (alreadyDumping) global.abort(msg)
    else alreadyDumping = true

    Console.println(msg)
    val printer = newTextPrinter()
    classes.values foreach printer.printClass
    global.abort(msg)
  }

  def dumpMethodAndAbort(m: IMethod, msg: String): Nothing = {
    Console.println("Fatal bug in inlinerwhile traversing " + m + ": " + msg)
    m.dump()
    global.abort("" + m)
  }
  def dumpMethodAndAbort(m: IMethod, b: BasicBlock): Nothing =
    dumpMethodAndAbort(m, "found open block " + b + " " + b.flagsString)

  def checkValid(m: IMethod) {
    // always slightly dicey to iterate over mutable structures
    m foreachBlock { b =>
      if (!b.closed) {
        // Something is leaving open/empty blocks around (see SI-4840) so
        // let's not kill the deal unless it's nonempty.
        if (b.isEmpty) {
          log("!!! Found open but empty block while inlining " + m + ": removing from block list.")
          m.code removeBlock b
        }
        else dumpMethodAndAbort(m, b)
      }
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

