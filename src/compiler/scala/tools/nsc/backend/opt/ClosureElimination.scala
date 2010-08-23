 /* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Iulian Dragos
 */


package scala.tools.nsc
package backend.opt;

import scala.collection.mutable.{Map, HashMap};
import scala.tools.nsc.backend.icode.analysis.LubException;
import scala.tools.nsc.symtab._;

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

  /** The Inlining phase.
   */
  class ClosureEliminationPhase(prev: Phase) extends ICodePhase(prev) {

    def name = phaseName
    val closser = new ClosureElim

    override def apply(c: IClass): Unit =
      closser.analyzeClass(c)
  }

  /**
   * Remove references to the environment through fields of a closure object.
   * This has to be run after an 'apply' method has been inlined, but it still
   * references the closure object.
   *
   */
  class ClosureElim {

    /* fresh name counter */
    var count = 0

    def freshName(s: String) = {
      val ret = s + this.count
      this.count += 1
      ret
    }

    /** A simple peephole optimizer. */
    val peephole = new PeepholeOpt( (i1, i2) =>
    	(i1, i2) match {
        case (CONSTANT(c), DROP(_)) =>
          if (c.tag == UnitTag)
            Some(List(i2))
          else
            Some(Nil);

        case (LOAD_LOCAL(x), STORE_LOCAL(y)) =>
        	if (x eq y) Some(Nil) else None

//        case (STORE_LOCAL(x), LOAD_LOCAL(y)) if (x == y) =>
//          Some(List(DUP(x.kind), STORE_LOCAL(x)))

        case (LOAD_LOCAL(_), DROP(_)) | (DUP(_), DROP(_)) =>
          Some(Nil)

        case (BOX(t1), UNBOX(t2)) if (t1 == t2) =>
          Some(Nil)

        case (LOAD_FIELD(sym, isStatic), DROP(_)) =>
        	if (isStatic)
            Some(Nil)
          else
            Some(DROP(REFERENCE(definitions.ObjectClass)) :: Nil);

        case _ => None
      });

    def analyzeClass(cls: IClass): Unit = if (settings.Xcloselim.value) {
      cls.methods.foreach { m =>
        analyzeMethod(m)
        peephole.transformMethod(m);
     }}


    val cpp = new copyPropagation.CopyAnalysis


    import copyPropagation._

    /* Some embryonic copy propagation. */
    def analyzeMethod(m: IMethod): Unit = try {if (m.code ne null) {
      log("Analyzing " + m)
      cpp.init(m)
      cpp.run

      for (bb <- linearizer.linearize(m)) {
        var info = cpp.in(bb)
        if (settings.debug.value) log("Cpp info at entry to block " + bb + ": " + info)

        for (i <- bb) {
          i match {
            case LOAD_LOCAL(l) if (info.bindings.isDefinedAt(LocalVar(l))) =>
              val t = info.getBinding(l)
              t match {
                case Deref(LocalVar(_)) | Deref(This) | Const(_) =>
                  bb.replaceInstruction(i, valueToInstruction(t));
                  log("replaced " + i + " with " + t)

                case _ =>
                  bb.replaceInstruction(i, LOAD_LOCAL(info.getAlias(l)));
                  log("replaced " + i + " with " + info.getAlias(l))

              }

            case LOAD_FIELD(f, false) /* if accessible(f, m.symbol) */ =>
              def replaceFieldAccess(r: Record) {
                val Record(cls, bindings) = r
                info.getFieldNonRecordValue(r, f) match {
                	case Some(v) =>
                		bb.replaceInstruction(i,
                				DROP(REFERENCE(cls)) :: valueToInstruction(v) :: Nil);
                		log("Replaced " + i + " with " + info.getFieldNonRecordValue(r, f));
                	case None => ();
                }
              }

              info.stack(0) match {
                case r @ Record(_, bindings) if bindings.isDefinedAt(f) =>
                  replaceFieldAccess(r)

                case Deref(LocalVar(l)) =>
                  info.getBinding(l) match {
                    case r @ Record(_, bindings) if bindings.isDefinedAt(f) =>
                      replaceFieldAccess(r)
                    case _ =>
                  }
                case Deref(Field(r1, f1)) =>
                  info.getFieldValue(r1, f1) match {
                    case Some(r @ Record(_, bindings)) if bindings.isDefinedAt(f) =>
                      replaceFieldAccess(r)
                    case _ =>
                  }

                case _ =>
              }

            case UNBOX(_) =>
              info.stack match {
                case Deref(LocalVar(loc1)) :: _ if (info.bindings.isDefinedAt(LocalVar(loc1))) =>
                  val value = info.getBinding(loc1)
                  value match {
                    case Boxed(LocalVar(loc2)) =>
                      bb.replaceInstruction(i, DROP(icodes.AnyRefReference) :: valueToInstruction(info.getBinding(loc2)) :: Nil)
                      log("replaced " + i + " with " + info.getBinding(loc2))
                    case _ =>
                      ()
                  }
                case Boxed(LocalVar(loc1)) :: _ =>
                  val value = info.getBinding(loc1)
                  bb.replaceInstruction(i, DROP(icodes.AnyRefReference) :: valueToInstruction(value) :: Nil)
                  log("replaced " + i + " with " + value)
                case _ =>
                  ()
              }

            case _ => ();
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
      f.isPublic || (f.hasFlag(Flags.PROTECTED) && (f.enclosingPackageClass == m.enclosingPackageClass))

  } /* class ClosureElim */


  /** Peephole optimization. */
  class PeepholeOpt(peep: (Instruction, Instruction) => Option[List[Instruction]]) {

    private var method: IMethod = null

    def transformMethod(m: IMethod): Unit = if (m.code ne null) {
      method = m
      for (b <- m.code.blocks)
        transformBlock(b)
    }

    def transformBlock(b: BasicBlock): Unit = if (b.size >= 2) {
      var newInstructions: List[Instruction] = Nil;

      newInstructions = b.toList

      var redo = false
      do {
        var h = newInstructions.head;
        var t = newInstructions.tail;
        var seen: List[Instruction] = Nil;
        redo = false;

        while (t != Nil) {
          peep(h, t.head) match {
            case Some(newInstrs) =>
              newInstructions = seen.reverse ::: newInstrs ::: t.tail;
              redo = true;
            case None =>
            	()
          }
          seen = h :: seen;
          h = t.head;
          t = t.tail
        }
      } while (redo);
      b.fromList(newInstructions)
    }
  }

} /* class ClosureElimination */
