/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Iulian Dragos
 */

// $Id: $

package scala.tools.nsc.backend.opt;

import scala.collection.mutable.{Map, HashMap};
import scala.tools.nsc.symtab._;

/**
 */
abstract class ClosureElimination extends SubComponent {
  import global._;
  import icodes._;
  import icodes.opcodes._;

  val phaseName = "closurelim";

  /** Create a new phase */
  override def newPhase(p: Phase) = new ClosureEliminationPhase(p);

  /** The Inlining phase.
   */
  class ClosureEliminationPhase(prev: Phase) extends GlobalPhase(prev) {
    def name = phaseName;
    override def newFlags = phaseNewFlags;

    override def erasedTypes = true;
    val closser = new ClosureElim;

    override def run: Unit = {
      if (settings.debug.value) inform("[running phase " + name + " on icode]");
      classes.values foreach closser.analyzeClass;
    }
    override def apply(unit: CompilationUnit): Unit =
      abort("Inlining works on icode classes, not on compilation units!");
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

    def analyzeClass(cls: IClass): Unit = if (settings.Xcloselim.value) {
      if (settings.debug.value)
      	log("Analyzing " + cls);
      cls.methods.foreach { m => analyzeMethod(m)
     }}


    val cpp = new copyPropagation.CopyAnalysis;


    import copyPropagation._;

    /* Some embryonic copy propagation. */
    def analyzeMethod(m: IMethod): Unit = if (m.code ne null) {
      Console.println("Analyzing " + m);
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
                  Console.println("replaced " + i + " with " + t);

                case This() =>
                  bb.replaceInstruction(i, valueToInstruction(t));
                  Console.println("replaced " + i + " with " + t);

                case _ =>
                  bb.replaceInstruction(i, LOAD_LOCAL(info.getAlias(l)));
                  Console.println("replaced " + i + " with " + info.getAlias(l));

              }

            case LOAD_FIELD(f, false) =>
              Console.println(" * * * \n" + i + "\n" + info);

              info.stack(0) match {
                case r @ Record(cls, bindings) if bindings.isDefinedAt(f) =>
                  bb.replaceInstruction(i,
                                        DROP(REFERENCE(cls)) ::
                                        valueToInstruction(info.getBinding(r, f)) :: Nil);
                  Console.println("Replaced " + i + " with " + info.getBinding(r, f));

                case Deref(LocalVar(l)) =>
                  info.getBinding(l) match {
                    case r @ Record(cls, bindings) if bindings.isDefinedAt(f) =>
                      bb.replaceInstruction(i,
                                            DROP(REFERENCE(cls)) ::
                                            valueToInstruction(info.getBinding(r, f)) :: Nil);
                      Console.println("Replaced " + i + " with " + info.getBinding(r, f));
                    case _ => ();
                  }
                  Console.println(info.getBinding(l));

                case _ => ();
              }

            case _ => ();
          }
          info = cpp.interpret(info, i);
        }
      }
    }

    /* Partial mapping from values to instructions that load them. */
    def valueToInstruction(v: Value): Instruction = v match {
      case Deref(LocalVar(v)) =>
        LOAD_LOCAL(v)

      case This() =>
        THIS(definitions.ObjectClass)
    }

  } /* class ClosureElim */


  /** Peephole optimization. */
/*  class PeepholeOpt(f: (Instruction, Instruction) => List[Instruction]) {

    private var method: IMethod = null;

    def transformMethod(m: IMethod): Unit = if (m.code ne null) {
      method = m;
      for (val b <- m.code.blocks)
        transformBlock(b);
    }

    def transformBlock(b: BasicBlock): Unit = {
      var newInstructions: List[Instruction] = Nil;
      var first: Instruction = null;
      var second: Instruction = null;

      def emit(i: Instruction) = {
        if (first == null)
          first = i;
        else if (second == null)
          second = i
        else {
          newInstructions = second :: newInstructions;
          first = second;
          second = i;
        }
      }

      for (val i <- b.toList) {
        if (first && second) {
          first = null; second = null;
          f(first, second) foreach emit;
        }
      }
    }
  }
*/
} /* class ClosureElimination */
