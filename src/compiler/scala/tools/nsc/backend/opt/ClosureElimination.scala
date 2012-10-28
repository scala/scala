 /* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Iulian Dragos
 */

package scala.tools.nsc
package backend.opt

import scala.tools.nsc.backend.icode.analysis.LubException
import scala.tools.nsc.symtab._

/**
 *  @author Iulian Dragos
 */
abstract class ClosureElimination extends SubComponent {
  import global._
  import icodes._
  import icodes.opcodes._

  val phaseName = "closelim"

  /** Create a new phase */
  override def newPhase(p: Phase) = new ClosureEliminationPhase(p)

  /** A simple peephole optimizer. */
  val peephole = new PeepholeOpt {

    def peep(bb: BasicBlock, i1: Instruction, i2: Instruction) = (i1, i2) match {
      case (CONSTANT(c), DROP(_)) =>
        if (c.tag == UnitTag) Some(List(i2)) else Some(Nil)

      case (LOAD_LOCAL(x), STORE_LOCAL(y)) =>
        if (x eq y) Some(Nil) else None

      case (STORE_LOCAL(x), LOAD_LOCAL(y)) if (x == y) =>
        var liveOut = liveness.out(bb)
        if (!liveOut(x)) {
          debuglog("store/load to a dead local? " + x)
          val instrs = bb.getArray
          var idx = instrs.length - 1
          while (idx > 0 && (instrs(idx) ne i2)) {
            liveOut = liveness.interpret(liveOut, instrs(idx))
            idx -= 1
          }
          if (!liveOut(x)) {
            log("Removing dead store/load of " + x.sym.initialize.defString)
            Some(Nil)
          } else None
        } else
          Some(List(DUP(x.kind), STORE_LOCAL(x)))

      case (LOAD_LOCAL(_), DROP(_)) | (DUP(_), DROP(_)) =>
        Some(Nil)

      case (BOX(t1), UNBOX(t2)) if (t1 == t2) =>
        Some(Nil)

      case (LOAD_FIELD(sym, isStatic), DROP(_)) if !sym.hasAnnotation(definitions.VolatileAttr) =>
        if (isStatic)
          Some(Nil)
        else
          Some(DROP(REFERENCE(definitions.ObjectClass)) :: Nil)

      case _ => None
    }
  }

  /** The closure elimination phase.
   */
  class ClosureEliminationPhase(prev: Phase) extends ICodePhase(prev) {

    def name = phaseName
    val closser = new ClosureElim

    override def apply(c: IClass): Unit =
      closser analyzeClass c
  }

  /**
   * Remove references to the environment through fields of a closure object.
   * This has to be run after an 'apply' method has been inlined, but it still
   * references the closure object.
   *
   */
  class ClosureElim {
    def analyzeClass(cls: IClass): Unit = if (settings.Xcloselim.value) {
      log(s"Analyzing ${cls.methods.size} methods in $cls.")
      cls.methods foreach { m =>
        analyzeMethod(m)
        peephole(m)
     }}

    val cpp = new copyPropagation.CopyAnalysis

    import copyPropagation._

    /* Some embryonic copy propagation. */
    def analyzeMethod(m: IMethod): Unit = try {if (m.hasCode) {
      cpp.init(m)
      cpp.run

      m.linearizedBlocks() foreach { bb =>
        var info = cpp.in(bb)
        debuglog("Cpp info at entry to block " + bb + ": " + info)

        for (i <- bb) {
          i match {
            case LOAD_LOCAL(l) if info.bindings isDefinedAt LocalVar(l) =>
              val t = info.getBinding(l)
              t match {
              	case Deref(This) | Const(_) =>
                  bb.replaceInstruction(i, valueToInstruction(t));
                  debuglog(s"replaced $i with $t")

                case _ =>
                  val t = info.getAlias(l)
                  bb.replaceInstruction(i, LOAD_LOCAL(t))
                  debuglog(s"replaced $i with $t")
              }

            case LOAD_FIELD(f, false) /* if accessible(f, m.symbol) */ =>
              def replaceFieldAccess(r: Record) {
                val Record(cls, bindings) = r
                info.getFieldNonRecordValue(r, f) foreach { v =>
                        bb.replaceInstruction(i, DROP(REFERENCE(cls)) :: valueToInstruction(v) :: Nil)
                        debuglog(s"replaced $i with $v")
                }
              }

              info.stack(0) match {
                case r @ Record(_, bindings) if bindings isDefinedAt f =>
                  replaceFieldAccess(r)

                case Deref(LocalVar(l)) =>
                  info.getBinding(l) match {
                    case r @ Record(_, bindings) if bindings isDefinedAt f =>
                      replaceFieldAccess(r)
                    case _ =>
                  }
                case Deref(Field(r1, f1)) =>
                  info.getFieldValue(r1, f1) match {
                    case Some(r @ Record(_, bindings)) if bindings isDefinedAt f =>
                      replaceFieldAccess(r)
                    case _ =>
                  }

                case _ =>
              }

            case UNBOX(boxType) =>
              info.stack match {
                case Deref(LocalVar(loc1)) :: _ if info.bindings isDefinedAt LocalVar(loc1) =>
                  val value = info.getBinding(loc1)
                  value match {
                    case Boxed(LocalVar(loc2)) if loc2.kind == boxType =>
                      bb.replaceInstruction(i, DROP(icodes.ObjectReference) :: valueToInstruction(info.getBinding(loc2)) :: Nil)
                      debuglog("replaced " + i + " with " + info.getBinding(loc2))
                    case _ =>
                      ()
                  }
                case Boxed(LocalVar(loc1)) :: _ if loc1.kind == boxType =>
                  val loc2 = info.getAlias(loc1)
                  bb.replaceInstruction(i, DROP(icodes.ObjectReference) :: valueToInstruction(Deref(LocalVar(loc2))) :: Nil)
                  debuglog("replaced " + i + " with " + LocalVar(loc2))
                case _ =>
              }

            case _ =>
          }
          info = cpp.interpret(info, i)
        }
      }
    }} catch {
      case e: LubException =>
        Console.println("In method: " + m)
        Console.println(e)
        e.printStackTrace
    }

    /* Partial mapping from values to instructions that load them. */
    def valueToInstruction(v: Value): Instruction = (v: @unchecked) match {
      case Deref(LocalVar(v)) =>
        LOAD_LOCAL(v)
      case Const(k) =>
        CONSTANT(k)
      case Deref(This) =>
        THIS(definitions.ObjectClass)
      case Boxed(LocalVar(v)) =>
        LOAD_LOCAL(v)
    }

    /** is field 'f' accessible from method 'm'? */
    def accessible(f: Symbol, m: Symbol): Boolean =
      f.isPublic || (f.isProtected && (f.enclosingPackageClass == m.enclosingPackageClass))
  } /* class ClosureElim */


  /** Peephole optimization. */
  abstract class PeepholeOpt {

    private var method: IMethod = NoIMethod

    /** Concrete implementations will perform their optimizations here */
    def peep(bb: BasicBlock, i1: Instruction, i2: Instruction): Option[List[Instruction]]

    var liveness: global.icodes.liveness.LivenessAnalysis = null

    def apply(m: IMethod): Unit = if (m.hasCode) {
      method = m
      liveness = new global.icodes.liveness.LivenessAnalysis
      liveness.init(m)
      liveness.run
      m foreachBlock transformBlock
    }

    def transformBlock(b: BasicBlock): Unit = if (b.size >= 2) {
      var newInstructions: List[Instruction] = b.toList
      var redo = false

      do {
        var h = newInstructions.head
        var t = newInstructions.tail
        var seen: List[Instruction] = Nil
        redo = false

        while (t != Nil) {
          peep(b, h, t.head) match {
            case Some(newInstrs) =>
              newInstructions = seen reverse_::: newInstrs ::: t.tail
              redo = true
            case None =>
            	()
          }
          seen = h :: seen
          h = t.head
          t = t.tail
        }
      } while (redo);
      b fromList newInstructions
    }
  }

} /* class ClosureElimination */
