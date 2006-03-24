package scala.tools.nsc.interdf
import scala.collection.mutable.{HashMap, HashSet, Queue}

/** Translation routine from ICode to DCode.
  *
  * To convert ICode to DCode, stack accesses must be
  * translated to StackVar accesses.  Pushes turn into
  * writes, while pops turn into reads.
  *
  * During the translation, the converter maintains abstract
  * stacks that specify which StackVar's at each program
  * point correspond to which stack positions.  These
  * stacks maintain the invariant that no variable
  * is used twice.
  *
  * Whenever an instruction pushes onto the stack, a new
  * StackVar is allocated.  Other choices seem reasonable,
  * but this one hopefully provides a moderate number of
  * variables that correspond to the structure of the original
  * program.
  *
  * During translation, whenever an icode block is visited
  * the first time, the translator records that block.  Upon
  * subsequent visits along a different incoming control
  * edge, the first stack and the new stack do not in general
  * agree.  To accodomodate this mismatch, (1) the first visit
  * translates the block according to the first seen stack, and
  * (2) later visits must prefix their entry to the block with
  * another block that rewrites the stack variables to conform
  * to the first visit.
  */

trait ICodeToDCode requires DCode {
  import compiler.{icodes => I}
  import I.{opcodes=>IOP}
  import I.UNIT
  import opcodes._
  import compiler.icodes.opcodes.{Static, Dynamic}

  /** Convert ICode into DCode. */
  def icodeToDCode(code: I.Code): Code = {
    /** Convert one ICode instruction to a DCode instruction */
    def convIns(ins: I.Instruction, stack: List[StackVar]): Pair[Instruction, List[StackVar]] = {
      ins match {
        case IOP.THIS(clasz) => {
          val lhs = genStackVar
          Pair(THIS(lhs, clasz),
               lhs::stack)
        }
        case IOP.CONSTANT(const) => {
          val lhs = genStackVar
          Pair(CONSTANT(lhs, const),
               lhs::stack)
        }
        case IOP.LOAD_ARRAY_ITEM(kind) => {
          val lhs = genStackVar
          val idx::ary::stkbase = stack
          Pair(LOAD_ARRAY_ITEM(lhs, ary, idx, kind),
               lhs::stkbase)
        }
        case IOP.LOAD_LOCAL(local, isArg) => {
          val lhs = genStackVar
          Pair(LOAD_LOCAL(lhs, local, isArg),
               lhs::stack)
        }
        case IOP.LOAD_FIELD(field, isStatic) => {
          val lhs = genStackVar
          val obj::stkbase = stack
          Pair(LOAD_FIELD(lhs, obj, field, isStatic),
               lhs::stkbase)
        }
        case IOP.LOAD_MODULE(module) => {
          val lhs = genStackVar
          Pair(LOAD_MODULE(lhs, module),
               lhs::stack)
        }
        case IOP.STORE_ARRAY_ITEM(kind) => {
          val obj::idx::ary::stkbase = stack
          Pair(STORE_ARRAY_ITEM(ary, idx, obj, kind),
               stkbase)
        }
        case IOP.STORE_LOCAL(local, isArg) => {
          val obj::stkbase = stack
          Pair(STORE_LOCAL(local, obj, isArg),
               stkbase)
        }
        case IOP.STORE_FIELD(field, isStatic) => {
          val obj::into::stkbase = stack
          Pair(STORE_FIELD(into, field, obj, isStatic),
               stkbase)
        }
        case IOP.CALL_PRIMITIVE(primitive) => {
          val lhs = genStackVar
          val numArgs = ins.consumed
          val args = stack.take(numArgs).reverse
          val stkbase = stack.drop(numArgs)
          Pair(CALL_PRIMITIVE(lhs, primitive, args),
               lhs::stkbase)
        }
        case IOP.CALL_METHOD(method, style) => {
          val lhs = genStackVar

          val numargs = method.tpe.paramTypes.length
          val args = stack.take(numargs).reverse
          val stack2 = stack.drop(numargs)

          val Pair(rcvr,stkbase) = style match {
            case Dynamic | Static(true) =>
              Pair(Some(stack2.head), stack2.tail)

            case _ => Pair(None, stack2)
          }

          Pair(CALL_METHOD(lhs, rcvr, method, args, style),
               lhs::stkbase)
        }
        case IOP.NEW(kind) => {
          val lhs = genStackVar
          Pair(NEW(lhs, kind),
               lhs::stack)
        }
        case IOP.CREATE_ARRAY(elem) => {
          val lhs = genStackVar
          val size::stkbase = stack
          Pair(CREATE_ARRAY(lhs, elem, size),
               lhs::stkbase)
        }
        case IOP.IS_INSTANCE(tpe) => {
          val lhs = genStackVar
          val rhs::stkbase = stack
          Pair(IS_INSTANCE(lhs, rhs, tpe),
               lhs::stkbase)
        }
        case IOP.CHECK_CAST(tpe) => {
          val lhs = genStackVar
          val rhs::stkbase = stack
          Pair(CHECK_CAST(lhs, rhs, tpe),
               lhs::stkbase)
        }
        case IOP.SWITCH(tags, labels) => {
          val obj::stkbase = stack
          Pair(SWITCH(obj, tags),
               stkbase)
        }
        case IOP.JUMP(where) => Pair(JUMP(), stack)
        case IOP.CJUMP(success, failure, cond, kind) => {
          val right::left::stkbase = stack
          Pair(CJUMP(left, cond, right, kind),
               stkbase)
        }
        case IOP.CZJUMP(success, failure, cond, kind) => {
          val obj::stkbase = stack
          Pair(CZJUMP(obj, cond, kind),
               stkbase)
        }
        case IOP.RETURN(kind) if kind==UNIT => {
          Pair(RETURN_VOID(kind), stack)
        }
        case IOP.RETURN(kind) => { // kind != UNIT
          val obj::stkbase = stack
          Pair(RETURN(obj, kind),
               stkbase)
        }
        case IOP.THROW() => {
          val exc::stkbase = stack
          Pair(THROW(exc),
               stkbase)
        }
        case IOP.DROP(kind) => {
          Pair(NOP, stack.tail)
        }
        case IOP.DUP(kind) => {
          val sv = genStackVar
          Pair(VAR(sv, stack.head),
               sv::stack)
        }
        case IOP.MONITOR_ENTER() => {
          val obj::stkbase = stack
          Pair(MONITOR_ENTER(obj),
               stkbase)
        }
        case IOP.MONITOR_EXIT() => {
          val obj::stkbase = stack
          Pair(MONITOR_EXIT(obj),
               stkbase)
        }
      }
    }

    /** Convert the instructions from an ICode block to a Triples block */
    def convInstrs(icblk: I.BasicBlock,
                   tripblk: BasicBlock,
                   startStack: List[StackVar]): List[StackVar] = {
      def convLoop(instrs: List[I.Instruction], stack: List[StackVar]):
              Pair[List[Instruction], List[StackVar]] = {
        instrs match {
          case Nil => Pair(Nil, stack)
          case ins::insRest => {
            val Pair(dins, nextStack) = convIns(ins, stack)
            ins match {
              case _:IOP.RETURN  => ()
              case _ => {
                if(!((nextStack.length - stack.length) ==  (ins.produced - ins.consumed))) {
                  Console.println("bad conversion!")
                  Console.println("Instruction: " + ins)
                  Console.println("in stack: " + stack)
                  Console.println("out stack: " + nextStack)
                  throw new Error("assertion failed")
                }
              }
            }
            val Pair(dinsRest, finalStack) = convLoop(insRest, nextStack)
            Pair(dins::dinsRest, finalStack)
          }
        }
      }
      val Pair(ins, outStack) = convLoop(icblk.toList, startStack)
      tripblk.instrs = ins
      outStack
    }

    /** compute a block for rewriting the variables of
      * one stack to the variables of another */
    def rewriteStack(before: List[StackVar], after: List[StackVar]): BasicBlock = {
      val beforeSet = new HashSet[StackVar]; beforeSet ++= before
      val firstInsrs = new Queue[Instruction]
      val lastInsrs = new Queue[Instruction]

      for{val Pair(b, a) <- before.zip(after)
          b != a}
      {
        if(!beforeSet.contains(a)) {
          // safe to kill 'a'
          firstInsrs += VAR(a, b)
        } else {
          // save 'a' to a temp variable, and only kill it at the end
          val tmpvar = genStackVar
          firstInsrs += VAR(tmpvar, b)
          lastInsrs += VAR(a, tmpvar)
        }
      }

      val insrs = firstInsrs.toList ::: lastInsrs.toList
      new BasicBlock(insrs, Nil)
    }

    /** Accumulates converted blocks */
    val blockMap = new HashMap[I.BasicBlock, BasicBlock]

    /** Records which input-stack was used the first time each ICode
      * block was converted to a DCode block.
      */
    val firstStackForBlock = new HashMap[BasicBlock, List[StackVar]]


    /** calculate the next pointers for a given block */
    def setNext(icblk: I.BasicBlock,
                tripblk: BasicBlock,
                endStack: List[StackVar]): Unit = {
      import compiler.icodes.opcodes._

      val out = icblk.lastInstruction match {
        case SWITCH(tags, labels) => labels
        case JUMP(where) => List(where)
        case CJUMP(success, failure, _, _) => List(success, failure)
        case CZJUMP(success, failure, _, _) => List(success, failure)
        case RETURN(kind) => Nil
        case THROW() => Nil
        case _ => throw new Error("basic block falls off the end")
      }
      tripblk.next = out.map(b => conv(b, endStack))
    }

    /** Convert one basic block to a BB buffer. Memoes using blockMap.*/
    def conv(blk: I.BasicBlock, stack: List[StackVar]): BasicBlock = {
      blockMap.get(blk) match {
        case Some(bb) => {
          // Already seen.  Add a translation block if necessary
          // in front of the previously converted block.
          val firstStack = firstStackForBlock(bb)
          assert(stack.length == firstStack.length)
          if(stack == firstStack)
            bb // same stacks -- no translation needed
          else {
            // different stacks; need to insert a translation block
            val trans = rewriteStack(stack, firstStack)
            trans.next = List(bb)
            trans
          }
        }
        case None => {
          // new translation
          val newblk = new BasicBlock
          blockMap.update(blk, newblk)  // record this *before* recursing via setNext()
          firstStackForBlock(newblk) = stack

          val outStack = convInstrs(blk, newblk, stack)
          setNext(blk, newblk, outStack)
          newblk
        }
      }
    }


    /** Find all blocks reachable from a specified block */
    def traverseAndCollect(b: BasicBlock): List[BasicBlock] = {
      val seen = new HashSet[BasicBlock]
      def trav(b: BasicBlock): List[BasicBlock] = {
        if(seen.contains(b))
          Nil
        else {
          seen += b
          b :: b.next.flatMap(trav)
        }
      }
      trav(b)
    }

    /* run the conversion */
    val start = conv(code.startBlock, Nil)
    val blocks = traverseAndCollect(start)
    new Code(blocks)
  }
}
