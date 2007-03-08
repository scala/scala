 /* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Iulian Dragos
 */

// $Id$

package scala.tools.nsc.backend.opt;

import scala.collection.mutable.{Map, HashMap};
import scala.tools.nsc.backend.icode.analysis.LubError;
import scala.tools.nsc.symtab._;

/**
 */
abstract class ClosureElimination extends SubComponent {
  import global._;
  import icodes._;
  import icodes.opcodes._;

  val phaseName = "closelim";

  /** Create a new phase */
  override def newPhase(p: Phase) = new ClosureEliminationPhase(p);

  /** The Inlining phase.
   */
  class ClosureEliminationPhase(prev: Phase) extends ICodePhase(prev) {

    def name = phaseName
    val closser = new ClosureElim;

    override def apply(c: IClass): Unit =
      closser.analyzeClass(c)
  }

  /**
   * Remove references to the environemnt through fields of a closure object.
   * This has to be run after an 'apply' method has been inlined, but it still
   * references the closure object.
   *
   */
  class ClosureElim {

    /* fresh name counter */
    var count = 0;

    def freshName(s: String) = {
      val ret = s + this.count;
      this.count = this.count + 1;
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

        case (LOAD_LOCAL(_), DROP(_)) =>
          Some(Nil)

        case (LOAD_FIELD(sym, isStatic), DROP(_)) =>
        	if (isStatic)
            Some(Nil)
          else
            Some(DROP(REFERENCE(definitions.ObjectClass)) :: Nil);

        case _ => None;
      });

    def analyzeClass(cls: IClass): Unit = if (settings.Xcloselim.value) {
      cls.methods.foreach { m =>
        analyzeMethod(m);
        peephole.transformMethod(m);
     }}


    val cpp = new copyPropagation.CopyAnalysis;


    import copyPropagation._;

    /* Some embryonic copy propagation. */
    def analyzeMethod(m: IMethod): Unit = try {if (m.code ne null) {
      log("Analyzing " + m);
      cpp.init(m);
      cpp.run;

      for (val bb <- linearizer.linearize(m)) {
        var info = cpp.in(bb);

        for (val i <- bb.toList) {
          i match {
            case LOAD_LOCAL(l) if (info.bindings.isDefinedAt(LocalVar(l))) =>
              val t = info.getBinding(l);
              t match {
                case Deref(LocalVar(v)) =>
                  bb.replaceInstruction(i, valueToInstruction(t));
                  log("replaced " + i + " with " + t);

                case This() =>
                  bb.replaceInstruction(i, valueToInstruction(t));
                  log("replaced " + i + " with " + t);

                case _ =>
                  bb.replaceInstruction(i, LOAD_LOCAL(info.getAlias(l)));
                  log("replaced " + i + " with " + info.getAlias(l));

              }

            case LOAD_FIELD(f, false) =>
              info.stack(0) match {
                case r @ Record(cls, bindings) if bindings.isDefinedAt(f) =>
                  info.getLocalForField(r, f) match {
                    case Some(local) =>
                      bb.replaceInstruction(i,
                          DROP(REFERENCE(cls)) :: valueToInstruction(local) :: Nil);
                      log("Replaced " + i + " with " + info.getBinding(r, f));
                    case None => ();
                  }

                case Deref(LocalVar(l)) =>
                  info.getBinding(l) match {
                    case r @ Record(cls, bindings) if bindings.isDefinedAt(f) =>
                      info.getLocalForField(r, f) match {
                        case Some(local) =>
                          bb.replaceInstruction(i,
                              DROP(REFERENCE(cls)) :: valueToInstruction(local) :: Nil);
                          log("Replaced " + i + " with " + info.getBinding(r, f));
                        case None => ();
                      }
                    case _ => ();
                  }

                case _ => ();
              }

            case _ => ();
          }
          info = cpp.interpret(info, i);
        }
      }
    }} catch {
      case e: LubError =>
        Console.println("In method: " + m);
        Console.println(e);
    }

    /* Partial mapping from values to instructions that load them. */
    def valueToInstruction(v: Value): Instruction = (v: @unchecked) match {
      case Deref(LocalVar(v)) =>
        LOAD_LOCAL(v)

      case This() =>
        THIS(definitions.ObjectClass)
    }

  } /* class ClosureElim */


  /** Peephole optimization. */
  class PeepholeOpt(peep: (Instruction, Instruction) => Option[List[Instruction]]) {

    private var method: IMethod = null;

    def transformMethod(m: IMethod): Unit = if (m.code ne null) {
      method = m;
      for (val b <- m.code.blocks)
        transformBlock(b);
    }

    def transformBlock(b: BasicBlock): Unit = if (b.size >= 2) {
      var newInstructions: List[Instruction] = Nil;

      newInstructions = b.toList;

      var redo = false;
      do {
        var h = newInstructions.head;
        var t = newInstructions.tail;
        var seen: List[Instruction] = Nil;
        redo = false;

        while (t != Nil) {
          peep(h, t.head) match {
            case Some(newInstrs) =>
            	Console.println("Replacing " + h + " : " + t.head + " by " + newInstrs);
              newInstructions = seen.reverse ::: newInstrs ::: t.tail;
              redo = true;
            case None =>
            	()
          }
          seen = h :: seen;
          h = t.head;
          t = t.tail;
        }
      } while (redo);
      b.fromList(newInstructions);
    }
  }

} /* class ClosureElimination */
