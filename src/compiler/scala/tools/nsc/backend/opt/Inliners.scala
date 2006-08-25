/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Iulian Dragos
 */

// $Id$

package scala.tools.nsc.backend.opt;

import scala.collection.mutable.{Map, HashMap};
import scala.tools.nsc.symtab._;

/**
 */
abstract class Inliners extends SubComponent {
  import global._;
  import RequiresIntsAsPositions._;
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

    /** Inline the 'callee' method inside the 'caller' in the given
     *  basic block, at the given instruction (which has to be a CALL_METHOD).
     */
    def inline(caller: IMethod,
               block:  BasicBlock,
               instr:  Instruction,
               callee: IMethod): Unit = {
       log("Inlining " + callee + " in " + caller + " at pos: " +
           classes(caller.symbol.owner).cunit.position(instr.pos));

       val targetPos = instr.pos;
       val a = new analysis.MethodTFA(callee);

       /* The exception handlers that are active at the current block. */
       val activeHandlers = caller.exh.filter(.covered.contains(block));

       /* Map 'original' blocks to the ones inlined in the caller. */
       val inlinedBlock: Map[BasicBlock, BasicBlock] = new HashMap;

       /* Map callee's parameters to inlined local variables. */
       val argsToLocal: Map[Local, Local] = new HashMap;

       val instrBefore = block.toList.takeWhile( i => i ne instr);
       val instrAfter  = block.toList.drop(instrBefore.length + 1);

       assert(!instrAfter.isEmpty, "CALL_METHOD cannot be the last instrcution in block!");

       // store the '$this' into the special local
       val inlinedThis = new Local(caller.symbol.newVariable(instr.pos, freshName("$inlThis")), REFERENCE(definitions.ObjectClass), false);

       /** buffer for the returned value */
       val retVal =
         if (callee.returnType != UNIT)
           new Local(caller.symbol.newVariable(instr.pos, freshName("$retVal")), callee.returnType, false);
         else
           null;

       /** Add a new block in the current context. */
       def newBlock = {
         val b = caller.code.newBlock;
         activeHandlers.foreach (.addBlock(b));
         b
       }

       def translateExh(e: ExceptionHandler) = {
         var handler: ExceptionHandler = e.dup;
         handler.covered = handler.covered.map(inlinedBlock);
         handler.setStartBlock(inlinedBlock(e.startBlock));
         handler
       }

       /** alfa-rename `l' in caller's context. */
       def dupLocal(l: Local): Local = {
         val sym = caller.symbol.newVariable(l.sym.pos, freshName(l.sym.name.toString()));
         sym.setInfo(l.sym.tpe);
         new Local(sym, l.kind, false)
       }

       /** Adds parameters from another method as locals */
       def addParamsAsLocals(m: IMethod, ls: List[Local]): Unit = {
         m.locals = m.locals ::: (ls map { a =>
           if (a.arg) {
             //val l = new Local(a.sym, a.kind, false);
             val l = dupLocal(a);
             argsToLocal += a -> l;
             l
           } else
             a
        });
       }

       def addLocals(m: IMethod, ls: List[Local]) =
         m.locals = m.locals ::: ls;
       def addLocal(m: IMethod, l: Local): Unit =
         addLocals(m, List(l));

       val afterBlock = newBlock;

       /** Map an instruction from the callee to one suitable for the caller. */
       def map(i: Instruction): Instruction = i match {
           case THIS(clasz) =>
             LOAD_LOCAL(inlinedThis);

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

           case LOAD_LOCAL(l) if (argsToLocal.isDefinedAt(l)) =>
             Console.println("Replacing load_local");
             LOAD_LOCAL(argsToLocal(l))

           case STORE_LOCAL(l) if (argsToLocal.isDefinedAt(l)) =>
             Console.println("Replacing store_local");
             STORE_LOCAL(argsToLocal(l))

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
       instrBefore.foreach(i => block.emit(i, i.pos));

       // store the arguments into special locals
       callee.params.reverse.foreach { param =>
         block.emit(STORE_LOCAL(param), targetPos);
       }
       block.emit(STORE_LOCAL(inlinedThis), targetPos);

       // jump to the start block of the callee
       block.emit(JUMP(inlinedBlock(callee.code.startBlock)), targetPos);
       block.close;

       // duplicate the other blocks in the callee
       linearizer.linearize(callee).foreach { bb =>
         var info = a.in(bb);
         bb traverse { i =>
           i match {
             case RETURN(kind) => kind match {
                 case UNIT =>
                   if (!info._2.types.isEmpty) {
                     info._2.types foreach { t => inlinedBlock(bb).emit(DROP(t), targetPos); }
                   }
                 case _ =>
                   if (info._2.length > 1) {
                     inlinedBlock(bb).emit(STORE_LOCAL(retVal), targetPos);
                     info._2.types.drop(1) foreach { t => inlinedBlock(bb).emit(DROP(t), targetPos); }
                     inlinedBlock(bb).emit(LOAD_LOCAL(retVal), targetPos);
                   }
               }
             case _ => ();
           }
           inlinedBlock(bb).emit(map(i), targetPos);
           info = a.interpret(info, i);
         }
         inlinedBlock(bb).close;
       }

       instrAfter.foreach(i => afterBlock.emit(i, i.pos));
       afterBlock.close;
       count = count + 1;

       // add exception handlers of the callee
       caller.exh = (callee.exh map translateExh) ::: caller.exh;
     }

    val InlineAttr = if (settings.inline.value) global.definitions.getClass("scala.inline").tpe else null;

    def analyzeClass(cls: IClass): Unit = if (settings.inline.value) {
      if (settings.debug.value)
      	log("Analyzing " + cls);
      cls.methods.foreach { m => analyzeMethod(m)
     }}


    val tfa = new analysis.MethodTFA();

    def analyzeMethod(m: IMethod): Unit = try {
      var retry = false;
      var count = 0;

      do {
        retry = false;
        if (m.code ne null) {
//          this.count = 0;
          if (settings.debug.value)
            log("Analyzing " + m + " count " + count);
          tfa.init(m);
          tfa.run;
          for (val bb <- linearizer.linearize(m)) {
            var info = tfa.in(bb);
            for (val i <- bb.toList) {
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
                      if (settings.debug.value)
                        log("" + i + " has actual receiver: " + receiver);
                    }
                    if (settings.debug.value)
                      log("Treating " + i);

                    if (   classes.contains(receiver)
                        && (isClosureClass(receiver)
                            || concreteMethod.isFinal
                            || msym.attributes.exists(a => a._1 == InlineAttr))) {
                      classes(receiver).lookupMethod(concreteMethod) match {
                        case Some(inc) =>
                          if (inc != m && (inc.code ne null)
                              && isSafeToInline(m, inc, info._2)) {
                            retry = true;
                            if (!isClosureClass(receiver)) // only count non-closures
                                count = count + 1;
                            inline(m, bb, i, inc);

                            /* Remove this method from the cache, as the calls-private relation
                               might have changed after the inlining. */
                            callsPrivate -= m;
                          }
                        case None =>
                          ();
                      }
                    }

                  case _ => ();
                }
                info = tfa.interpret(info, i);
              }}}}
      } while (retry && count < 15);
      normalize(m);
    } catch {
      case e =>
        Console.println("############# Cought exception: " + e + " #################");
        Console.println("\nMethod: " + m +
                        "\nMethod owner: " + m.symbol.owner);
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
      res
    }

    /** Cache whether a method calls private members. */
    val callsPrivate: Map[IMethod, Boolean] = new HashMap;

    /** A method is safe to inline when:
     *    - it does not contain calls to private methods when
     *      called from another class
     *    - it is not inlined into a position with non-empty stack,
     *      while having a top-level finalizer (see liftedTry problem)
     * Note:
     *    - synthetic private members are made public in this pass.
     */
    def isSafeToInline(caller: IMethod, callee: IMethod, stack: TypeStack): Boolean = {
      var callsPrivateMember = false;

      callsPrivate get (callee) match {
        case Some(b) => callsPrivateMember = b;
        case None =>
          for (val b <- callee.code.blocks)
            for (val i <- b.toList)
              i match {
                case CALL_METHOD(m, style) =>
                  if (m.hasFlag(Flags.PRIVATE) ||
                      (style.isSuper && !m.isClassConstructor))
                    callsPrivateMember = true;

                case LOAD_FIELD(f, _) =>
                  if (f.hasFlag(Flags.PRIVATE))
                    if (f.hasFlag(Flags.SYNTHETIC) || f.hasFlag(Flags.PARAMACCESSOR)) {
                      if (settings.debug.value)
                        log("Making not-private symbol out of synthetic: " + f);
                      f.setFlag(Flags.notPRIVATE)
                    } else
                      callsPrivateMember = true;

                case STORE_FIELD(f, _) =>
                  if (f.hasFlag(Flags.PRIVATE))
                    if (f.hasFlag(Flags.SYNTHETIC) || f.hasFlag(Flags.PARAMACCESSOR)) {
                      if (settings.debug.value)
                        log("Making not-private symbol out of synthetic: " + f);
                      f.setFlag(Flags.notPRIVATE)
                    } else
                      callsPrivateMember = true;

                case _ => ()
              }
          callsPrivate += callee -> callsPrivateMember;
        }

      if (callsPrivateMember && (caller.symbol.owner != callee.symbol.owner))
        return false;

      if (stack.length > (1 + callee.symbol.info.paramTypes.length) &&
          (callee.exh exists (.covered.contains(callee.code.startBlock)))) {
        if (settings.debug.value) log("method " + callee.symbol + " is used on a non-empty stack with finalizer.");
        false
      } else
        true
    }

  } /* class Inliner */
} /* class Inliners */
