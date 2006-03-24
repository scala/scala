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

    /* fresh name counter */
    var count = 0;

    def freshName(s: String) = {
      val ret = s + this.count;
      this.count = this.count + 1;
      ret
    }

    def inline(caller: IMethod,
               block:  BasicBlock,
               instr:  Instruction,
               callee: IMethod): Unit = {
       log("Inlining " + callee + " in " + caller + " at pos: " +
           classes(caller.symbol.owner).cunit.position(instr.pos));

       val a = new analysis.MethodTFA(callee);

       /* The exception handlers that are active at the current block. */
       val activeHandlers = caller.exh.filter(.covered.contains(block));

       /* Map 'original' blocks to the ones inlined in the caller. */
       val inlinedBlock: Map[BasicBlock, BasicBlock] = new HashMap;

       val instrBefore = block.toList.takeWhile( i => i != instr);
       val instrAfter  = block.toList.drop(instrBefore.length + 1);

       assert(!instrAfter.isEmpty, "CALL_METHOD cannot be the last instrcution in block!");

       // store the '$this' into the special local
       val inlinedThis = new Local(caller.symbol.newVariable(instr.pos, freshName("$inlThis")), REFERENCE(definitions.ObjectClass));

       /** buffer for the returned value */
       val retVal =
         if (callee.returnType != UNIT)
           new Local(caller.symbol.newVariable(instr.pos, freshName("$retVal")), callee.returnType);
         else
           null;

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

           case SWITCH(tags, labels) =>
             SWITCH(tags, labels map inlinedBlock);

           case RETURN(kind) =>
             JUMP(afterBlock);

           case _ => i
         }

       addLocals(caller, callee.locals);
       addLocal(caller, inlinedThis);
       if (retVal ne null)
         addLocal(caller, retVal);
       callee.code.blocks.foreach { b =>
         inlinedBlock += b -> newBlock;
       }

       // analyse callee
       a.run;

       // re-emit the instructions before the call
       block.open;
       block.clear;
       instrBefore.foreach(block.emit);

       // store the arguments into special locals
       callee.params.reverse.foreach { param =>
         block.emit(STORE_LOCAL(param, false));
       }
       block.emit(STORE_LOCAL(inlinedThis, false));

       // jump to the start block of the callee
       block.emit(JUMP(inlinedBlock(callee.code.startBlock)));
       block.close;

       // duplicate the other blocks in the callee
       linearizer.linearize(callee).foreach { bb =>
         var info = a.in(bb);
         bb traverse { i =>
           i match {
             case RETURN(kind) => kind match {
                 case UNIT =>
                 	 if (!info._2.types.isEmpty) {
                 	   log("** Dumping useless stack elements");
                 	   info._2.types foreach { t => inlinedBlock(bb).emit(DROP(t)); }
                   }
                 case _ =>
                   if (info._2.length > 1) {
                     log("** Dumping useless stack elements");
                     inlinedBlock(bb).emit(STORE_LOCAL(retVal, false));
                     info._2.types.drop(1) foreach { t => inlinedBlock(bb).emit(DROP(t)); }
                     inlinedBlock(bb).emit(LOAD_LOCAL(retVal, false));
                   }
               }
             case _ => ();
           }
           inlinedBlock(bb).emit(map(i), 0);
           info = a.interpret(info, i);
         }
         inlinedBlock(bb).close;
       }

       instrAfter.foreach(afterBlock.emit);
       afterBlock.close;
       count = count + 1;
     }


    /** Add a local to this method, alfa-renaming is not
     *  necessary because we use symbols to refer to locals.
     */
    def addLocals(m: IMethod, ls: List[Local]): Unit = {
      m.locals = m.locals ::: ls;
    }

    def addLocal(m: IMethod, l: Local): Unit =
      m.locals = m.locals ::: List(l);

    val InlineAttr = if (settings.inline.value) global.definitions.getClass("scala.inline").tpe else null;

    def analyzeClass(cls: IClass): Unit = if (settings.inline.value) {
      	log("Analyzing " + cls);
        cls.methods.foreach { m => analyzeMethod(m)
     }}


    def analyzeMethod(m: IMethod): Unit = try {
      var retry = false;
      var count = 0;

      do {
        retry = false;
        if (m.code ne null) {
          this.count = 0;
          if (settings.debug.value)
            log("Analyzing " + m + " count " + count);
          val a = new analysis.MethodTFA(m);
          a.run;
          linearizer.linearize(m).foreach { bb =>
            var info = a.in(bb);
            bb.traverse { i =>
              if (!retry) {
                i match {
                  case CALL_METHOD(msym, Dynamic) =>
                    val receiver = info._2.types.drop(msym.info.paramTypes.length).head match {
                      case REFERENCE(s) => s;
                      case _ => NoSymbol;
                    }
                    var concreteMethod = msym;
                    if (receiver != msym.owner && receiver != NoSymbol) {
                      concreteMethod = msym.overridingSymbol(receiver);
                      log("" + i + " has actual receiver: " + receiver);
                    }

                    if (   classes.contains(receiver)
                        && (isClosureClass(receiver)
                            || concreteMethod.isFinal
                            || msym.attributes.exists(a => a._1 == InlineAttr))) {
                      classes(receiver).lookupMethod(concreteMethod) match {
                        case Some(inc) =>
                          if (inc != m && (inc.code ne null)
                              && isSafeToInline(m, inc, info._2)) {
                            retry = true;
                            count = count + 1;
                            inline(m, bb, i, inc);
                          }
                        case None =>
                          log("Couldn't find " + msym.name);
                      }
                    }

                  case _ => ();
                }
                info = a.interpret(info, i);
              }}}}
      } while (retry && count < 5);
    } catch {
      case e =>
        Console.println("############# Cought exception: " + e + " #################");
        e.printStackTrace();
        dump(m);
        throw e;
    }

    def isClosureClass(cls: Symbol): Boolean = {
      val res =
        cls.isFinal &&
        cls.tpe.parents.exists { t =>
          val TypeRef(_, sym, _) = t;
          definitions.FunctionClass exists sym.==
        }
      Console.println("isClosureClass: " + cls + " is: " + res);
      res
    }


    /** A method is safe to inline when:
     *    - it does not contain calls to private methods when
     *      called from another class
     *    - it is not inlined into a position with non-empty stack,
     *      while having a top-level finalizer (see liftedTry problem)
     */
    def isSafeToInline(caller: IMethod, callee: IMethod, stack: TypeStack): Boolean = {
      if (caller.symbol.owner != callee.symbol.owner) {
        var callsPrivateMember = false;
        for (val b <- callee.code.blocks)
          for (val i <- b.toList)
            i match {
              case CALL_METHOD(m, style) =>
                if (m.hasFlag(Flags.PRIVATE) || style.isSuper)
                  callsPrivateMember = true;
              case LOAD_FIELD(f, _) =>
                if (f.hasFlag(Flags.PRIVATE)) callsPrivateMember = true;
              case _ => ()
            }
        if (callsPrivateMember) return false;
      }

      if (stack.length > (1 + callee.symbol.info.paramTypes.length) &&
          (callee.exh exists (.covered.contains(callee.code.startBlock))))
        false;
      else
        true
    }

  } /* class Inliner */
} /* class Inliners */
