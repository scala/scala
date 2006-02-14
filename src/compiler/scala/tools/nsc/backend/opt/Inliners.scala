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
abstract class Inliners extends SubComponent {
  import global._;
  import icodes._;
  import icodes.opcodes._;

  val phaseName = "inliner";

  /** Create a new phase */
  override def newPhase(p: Phase) = new InliningPhase(p);

  /** The Inlining phase.
   */
  class InliningPhase(prev: Phase) extends GlobalPhase(prev) {
    def name = phaseName;
    override def newFlags = phaseNewFlags;

    override def erasedTypes = true;
    val inliner = new Inliner;

    override def run: Unit = {
      if (settings.debug.value) inform("[running phase " + name + " on icode]");
      classes.values foreach inliner.analyzeClass;
    }
    override def apply(unit: CompilationUnit): Unit =
      abort("Inlining works on icode classes, not on compilation units!");
  }

  /**
   * Simple inliner.
   *
   */
  class Inliner {

    def inline(caller: IMethod,
               block:  BasicBlock,
               instr:  Instruction,
               callee: IMethod): Unit = {
       if (settings.debug.value)
         log("Inlining " + callee + " in " + caller + " at pos: " +
             classes(caller.symbol.owner).cunit.position(instr.pos));

       /* The exception handlers that are active at the current block. */
       val activeHandlers = caller.exh.filter(.covered.contains(block));

       /* Map 'original' blocks to the ones inlined in the caller. */
       val inlinedBlock: Map[BasicBlock, BasicBlock] = new HashMap;

       val instrBefore = block.instructions.takeWhile( i => i != instr);
       val instrAfter  = block.instructions.drop(instrBefore.length + 1);

       if (settings.debug.value) {
         log("instrBefore: " + instrBefore);
         log("instrAfter: " + instrAfter);
       }
       assert(!instrAfter.isEmpty, "CALL_METHOD cannot be the last instrcution in block!");

       // store the '$this' into the special local
       val inlinedThis = new Local(caller.symbol.newVariable(instr.pos,"$inlThis"), REFERENCE(definitions.ObjectClass));

       /** Add a new block in the current context. */
       def newBlock = {
         val b = caller.code.newBlock;
         activeHandlers.foreach (.addBlock(b));
         b
       }

       val afterBlock = newBlock;

       /** Map an instruction from the callee to one suitable for the caller. */
       def map(i: Instruction): Instruction = i match {
           case THIS(clasz) =>
             LOAD_LOCAL(inlinedThis, false);

           case LOAD_LOCAL(local, isArg) if (isArg) =>
             LOAD_LOCAL(local, false);

           case STORE_LOCAL(local, isArg) if (isArg) =>
             STORE_LOCAL(local, false);

           case JUMP(where) =>
             JUMP(inlinedBlock(where));

           case CJUMP(success, failure, cond, kind) =>
             CJUMP(inlinedBlock(success), inlinedBlock(failure), cond, kind);

           case CZJUMP(success, failure, cond, kind) =>
             CZJUMP(inlinedBlock(success), inlinedBlock(failure), cond, kind);

           case RETURN(kind) =>
             JUMP(afterBlock);

           case _ => i
         }

       addLocals(caller, callee.locals);
       addLocal(caller, inlinedThis);
       callee.code.blocks.foreach { b =>
         if (b != callee.code.startBlock)
           inlinedBlock += b -> newBlock;
       }

       // re-emit the instructions before the call
       block.open;
       block.clear;
       instrBefore.foreach(block.emit);

       // store the arguments into special locals
       callee.params.reverse.foreach { param =>
         block.emit(STORE_LOCAL(param, false));
       }
       block.emit(STORE_LOCAL(inlinedThis, false));

       // inline the start block of the callee
       callee.code.startBlock.traverse { i =>
         block.emit(map(i), 0);
       }
       block.close;

       // duplicate the other blocks in the callee
       callee.code.traverse { bb =>
         if (bb != callee.code.startBlock) {
           bb.traverse( i => inlinedBlock(bb).emit(map(i), 0) );
           inlinedBlock(bb).close;
         }
       }

       instrAfter.foreach(afterBlock.emit);
       afterBlock.close;
     }


    /** Add a local to this method, performing alfa-renaming
     *  if necessary.
     */
    def addLocals(m: IMethod, ls: List[Local]): Unit = {
      m.locals = m.locals ::: ls;
    }

    def addLocal(m: IMethod, l: Local): Unit =
      m.locals = m.locals ::: List(l);

    val InlineAttr = if (settings.inline.value) global.definitions.getClass("scala.inline").tpe;

    def analyzeClass(cls: IClass): Unit = if (settings.inline.value) {
        cls.methods.foreach { m => analyzeMethod(m)
     }}


    def analyzeMethod(m: IMethod): Unit = {
      var retry = false;
      var count = 0;

      do {
        retry = false;
        if (m.code ne null)
          m.code.traverse { bb =>
            bb.traverse { i =>
              if (!retry)
                i match {
                  case CALL_METHOD(msym, _) =>
                    if (definitions.isFunctionType(msym.owner.tpe)
                        || msym.attributes.exists(a => a._1 == InlineAttr))
                      classes(msym.owner).lookupMethod(msym.name) match {
                        case Some(inc) =>
                          retry = true;
                          count = count + 1;
                          inline(m, bb, i, inc);
                        case None =>
                          log("Couldn't find " + msym.name);
                      }
                  case _ => ();
                }}}
      } while (retry && count < 5);
    }
  }
}
