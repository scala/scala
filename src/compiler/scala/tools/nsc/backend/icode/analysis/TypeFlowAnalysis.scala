/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package backend.icode.analysis

import scala.collection.{mutable, immutable}

/** A data-flow analysis on types, that works on <code>ICode</code>.
 *
 *  @author Iulian Dragos
 */
abstract class TypeFlowAnalysis {
  val global: Global
  import global._

  /** The lattice of ICode types.
   */
  object typeLattice extends CompleteLattice {
    type Elem = icodes.TypeKind

    val Object = icodes.REFERENCE(global.definitions.ObjectClass)
    val All    = icodes.REFERENCE(global.definitions.NothingClass)

    def top    = Object
    def bottom = All

    def lub2(exceptional: Boolean)(a: Elem, b: Elem) =
      if (a eq bottom) b
      else if (b eq bottom) a
      else icodes.lub(a, b)
  }

  /** The lattice of type stacks. It is a straight forward extension of
   *  the type lattice (lub is pairwise lub of the list elements).
   */
  object typeStackLattice extends CompleteLattice {
    import icodes._
    type Elem = TypeStack

    override val top    = new TypeStack
    override val bottom = new TypeStack
    val exceptionHandlerStack: TypeStack = new TypeStack(List(REFERENCE(definitions.AnyRefClass)))

    def lub2(exceptional: Boolean)(s1: TypeStack, s2: TypeStack) = {
      if (s1 eq bottom) s2
      else if (s2 eq bottom) s1
      else if ((s1 eq exceptionHandlerStack) || (s2 eq exceptionHandlerStack)) Predef.error("merging with exhan stack")
      else {
//        if (s1.length != s2.length)
//          throw new CheckerError("Incompatible stacks: " + s1 + " and " + s2);
        new TypeStack(List.map2(s1.types, s2.types) (icodes.lub))
      }
    }
  }

  /** A map which returns the bottom type for unfound elements */
  class VarBinding extends mutable.HashMap[icodes.Local, icodes.TypeKind] {
    override def get(l: icodes.Local) = super.get(l) match {
      case Some(t) => Some(t)
      case None    => Some(typeLattice.bottom)
    }

    def this(o: VarBinding) = {
      this()
      this ++= o
    }
  }

  /** The type flow lattice contains a binding from local variable
   *  names to types and a type stack.
   */
  object typeFlowLattice extends CompleteLattice {
    import icodes._
    type Elem = IState[VarBinding, icodes.TypeStack]

    override val top    = new Elem(new VarBinding, typeStackLattice.top)
    override val bottom = new Elem(new VarBinding, typeStackLattice.bottom)

    def lub2(exceptional: Boolean)(a: Elem, b: Elem) = {
      val IState(env1, s1) = a
      val IState(env2, s2) = b

      val resultingLocals = new VarBinding

      for (binding1 <- env1.iterator) {
        val tp2 = env2(binding1._1)
        resultingLocals += ((binding1._1, typeLattice.lub2(exceptional)(binding1._2, tp2)))
      }

      for (binding2 <- env2.iterator if resultingLocals(binding2._1) eq typeLattice.bottom) {
        val tp1 = env1(binding2._1)
        resultingLocals += ((binding2._1, typeLattice.lub2(exceptional)(binding2._2, tp1)))
      }

      IState(resultingLocals,
        if (exceptional) typeStackLattice.exceptionHandlerStack
        else typeStackLattice.lub2(exceptional)(a.stack, b.stack))
    }
  }

  val timer = new Timer

  class MethodTFA extends DataFlowAnalysis[typeFlowLattice.type] {
    import icodes._
    import icodes.opcodes._

    type P = BasicBlock
    val lattice = typeFlowLattice

    val STRING = icodes.REFERENCE(TypeFlowAnalysis.this.global.definitions.StringClass)
    var method: IMethod = _

    /** Initialize the in/out maps for the analysis of the given method. */
    def init(m: icodes.IMethod) {
      this.method = m

      init {
        worklist += m.code.startBlock
        worklist ++= (m.exh map (_.startBlock))
        m.code.blocks.foreach { b =>
          in(b)  = typeFlowLattice.bottom
          out(b) = typeFlowLattice.bottom
        }
        m.exh foreach { e =>
          in(e.startBlock) = lattice.IState(in(e.startBlock).vars, typeStackLattice.exceptionHandlerStack)
        }
      }
    }

    /** reinitialize the analysis, keeping around solutions from a previous run. */
    def reinit(m: icodes.IMethod) {
      if ((this.method eq null) || (this.method.symbol != m.symbol))
        init(m)
      else reinit {
        for (b <- m.code.blocks; if !in.isDefinedAt(b)) {
          for (p <- b.predecessors) {
            if (out.isDefinedAt(p)) {
              in(b) = out(p)
              worklist += p
            }
/*            else
              in(b)  = typeFlowLattice.bottom
*/          }
          out(b) = typeFlowLattice.bottom
        }
        for (exh <- m.exh; if !in.isDefinedAt(exh.startBlock)) {
          worklist += exh.startBlock
          in(exh.startBlock) = lattice.IState(in(exh.startBlock).vars, typeStackLattice.exceptionHandlerStack)
        }
      }
    }

    def this(m: icodes.IMethod) {
      this()
      init(m)
    }

    def run = {
      timer.start
      forwardAnalysis(blockTransfer)
      timer.stop
      if (settings.debug.value) {
        linearizer.linearize(method).foreach(b => if (b != method.code.startBlock)
          assert(visited.contains(b),
            "Block " + b + " in " + this.method + " has input equal to bottom -- not visited? .." + visited));
      }
      //println("iterations: " + iterations + " for " + method.code.blocks.size)
    }

    def blockTransfer(b: BasicBlock, in: lattice.Elem): lattice.Elem = {
      b.foldLeft(in)(interpret)
    }

    /** The flow function of a given basic block. */
    var flowFun: immutable.Map[BasicBlock, TransferFunction] = new immutable.HashMap

    /** Fill flowFun with a transfer function per basic block. */
/*    private def buildFlowFunctions(blocks: List[BasicBlock]) {
      def transfer(b: BasicBlock): TransferFunction = {
        var gens: List[Gen] = Nil
        var consumed: Int = 0
        val stack = new SimulatedStack

        for (instr <- b) instr match {
        case THIS(clasz) =>
          stack push toTypeKind(clasz.tpe)

        case CONSTANT(const) =>
          stack push toTypeKind(const.tpe)

        case LOAD_ARRAY_ITEM(kind) =>
          stack.pop2
          stack.push(kind)

        case LOAD_LOCAL(local) =>
          val t = bindings(local)
          stack push (if (t == typeLattice.bottom) local.kind  else t)

        case LOAD_FIELD(field, isStatic) =>
          if (!isStatic)
            stack.pop
          stack push toTypeKind(field.tpe)

        case LOAD_MODULE(module) =>
          stack push toTypeKind(module.tpe)

        case STORE_ARRAY_ITEM(kind) =>
          stack.pop3

        case STORE_LOCAL(local) =>
          val t = stack.pop
          bindings += (local -> t)

        case STORE_THIS(_) =>
          stack.pop

        case STORE_FIELD(field, isStatic) =>
          if (isStatic)
            stack.pop
          else
            stack.pop2

        case CALL_PRIMITIVE(primitive) =>
          primitive match {
            case Negation(kind) =>
              stack.pop; stack.push(kind)
            case Test(_, kind, zero) =>
              stack.pop
              if (!zero) stack.pop
              stack push BOOL;
            case Comparison(_, _) =>
              stack.pop2
              stack push INT

            case Arithmetic(op, kind) =>
              stack.pop
              if (op != NOT)
                stack.pop
              val k = kind match {
                case BYTE | SHORT | CHAR => INT
                case _ => kind
              }
              stack push k

            case Logical(op, kind) =>
              stack.pop2
              stack push kind

            case Shift(op, kind) =>
              stack.pop2
              stack push kind

            case Conversion(src, dst) =>
              stack.pop
              stack push dst

            case ArrayLength(kind) =>
              stack.pop
              stack push INT

            case StartConcat =>
              stack.push(ConcatClass)

            case EndConcat =>
              stack.pop
              stack.push(STRING)

            case StringConcat(el) =>
              stack.pop2
              stack push ConcatClass
          }

        case CALL_METHOD(method, style) => style match {
          case Dynamic =>
            stack.pop(1 + method.info.paramTypes.length)
            stack.push(toTypeKind(method.info.resultType))

          case Static(onInstance) =>
            if (onInstance) {
              stack.pop(1 + method.info.paramTypes.length)
              if (!method.isConstructor)
                stack.push(toTypeKind(method.info.resultType));
            } else {
              stack.pop(method.info.paramTypes.length)
              stack.push(toTypeKind(method.info.resultType))
            }

          case SuperCall(mix) =>
            stack.pop(1 + method.info.paramTypes.length)
            stack.push(toTypeKind(method.info.resultType))
        }

        case BOX(kind) =>
          stack.pop
          stack.push(BOXED(kind))

        case UNBOX(kind) =>
          stack.pop
          stack.push(kind)

        case NEW(kind) =>
          stack.push(kind)

        case CREATE_ARRAY(elem, dims) =>
          stack.pop(dims)
          stack.push(ARRAY(elem))

        case IS_INSTANCE(tpe) =>
          stack.pop
          stack.push(BOOL)

        case CHECK_CAST(tpe) =>
          stack.pop
          stack.push(tpe)

        case SWITCH(tags, labels) =>
          stack.pop

        case JUMP(whereto) =>
          ()

        case CJUMP(success, failure, cond, kind) =>
          stack.pop2

        case CZJUMP(success, failure, cond, kind) =>
          stack.pop

        case RETURN(kind) =>
          if (kind != UNIT)
            stack.pop;

        case THROW() =>
          stack.pop

        case DROP(kind) =>
          stack.pop

        case DUP(kind) =>
          stack.push(stack.head)

        case MONITOR_ENTER() =>
          stack.pop

        case MONITOR_EXIT() =>
          stack.pop

        case SCOPE_ENTER(_) | SCOPE_EXIT(_) =>
          ()

        case LOAD_EXCEPTION() =>
          stack.pop(stack.length)
          stack.push(typeLattice.Object)

        case _ =>
          dump
          abort("Unknown instruction: " + i)

        }

        new TransferFunction(consumed, gens)
      }

      for (b <- blocks) {
        flowFun = flowFun + (b -> transfer(b))
      }
    }
*/
    /** Abstract interpretation for one instruction. */
    def interpret(in: typeFlowLattice.Elem, i: Instruction): typeFlowLattice.Elem = {
      val out = lattice.IState(new VarBinding(in.vars), new TypeStack(in.stack))
      val bindings = out.vars
      val stack = out.stack

      if (settings.debug.value) {
        Console.println("[before] Stack: " + stack);
        Console.println(i);
      }
      i match {

        case THIS(clasz) =>
          stack push toTypeKind(clasz.tpe)

        case CONSTANT(const) =>
          stack push toTypeKind(const.tpe)

        case LOAD_ARRAY_ITEM(kind) =>
          stack.pop2 match {
            case (idxKind, ARRAY(elem)) =>
              assert(idxKind == INT || idxKind == CHAR || idxKind == SHORT || idxKind == BYTE)
              stack.push(elem)
            case (_, _) =>
              stack.push(kind)
          }

        case LOAD_LOCAL(local) =>
          val t = bindings(local)
          stack push (if (t == typeLattice.bottom) local.kind  else t)

        case LOAD_FIELD(field, isStatic) =>
          if (!isStatic)
            stack.pop
          stack push toTypeKind(field.tpe)

        case LOAD_MODULE(module) =>
          stack push toTypeKind(module.tpe)

        case STORE_ARRAY_ITEM(kind) =>
          stack.pop3

        case STORE_LOCAL(local) =>
          val t = stack.pop
          bindings += (local -> t)

        case STORE_THIS(_) =>
          stack.pop

        case STORE_FIELD(field, isStatic) =>
          if (isStatic)
            stack.pop
          else
            stack.pop2

        case CALL_PRIMITIVE(primitive) =>
          primitive match {
            case Negation(kind) =>
              stack.pop; stack.push(kind)
            case Test(_, kind, zero) =>
              stack.pop
              if (!zero) stack.pop
              stack push BOOL;
            case Comparison(_, _) =>
              stack.pop2
              stack push INT

            case Arithmetic(op, kind) =>
              stack.pop
              if (op != NOT)
                stack.pop
              val k = kind match {
                case BYTE | SHORT | CHAR => INT
                case _ => kind
              }
              stack push k

            case Logical(op, kind) =>
              stack.pop2
              stack push kind

            case Shift(op, kind) =>
              stack.pop2
              stack push kind

            case Conversion(src, dst) =>
              stack.pop
              stack push dst

            case ArrayLength(kind) =>
              stack.pop
              stack push INT

            case StartConcat =>
              stack.push(ConcatClass)

            case EndConcat =>
              stack.pop
              stack.push(STRING)

            case StringConcat(el) =>
              stack.pop2
              stack push ConcatClass
          }

        case CALL_METHOD(method, style) => style match {
          case Dynamic | InvokeDynamic =>
            stack.pop(1 + method.info.paramTypes.length)
            stack.push(toTypeKind(method.info.resultType))

          case Static(onInstance) =>
            if (onInstance) {
              stack.pop(1 + method.info.paramTypes.length)
              if (!method.isConstructor)
                stack.push(toTypeKind(method.info.resultType));
            } else {
              stack.pop(method.info.paramTypes.length)
              stack.push(toTypeKind(method.info.resultType))
            }

          case SuperCall(mix) =>
            stack.pop(1 + method.info.paramTypes.length)
            stack.push(toTypeKind(method.info.resultType))
        }

        case BOX(kind) =>
          stack.pop
          stack.push(BOXED(kind))

        case UNBOX(kind) =>
          stack.pop
          stack.push(kind)

        case NEW(kind) =>
          stack.push(kind)

        case CREATE_ARRAY(elem, dims) =>
          stack.pop(dims)
          stack.push(ARRAY(elem))

        case IS_INSTANCE(tpe) =>
          stack.pop
          stack.push(BOOL)

        case CHECK_CAST(tpe) =>
          stack.pop
          stack.push(tpe)

        case SWITCH(tags, labels) =>
          stack.pop

        case JUMP(whereto) =>
          ()

        case CJUMP(success, failure, cond, kind) =>
          stack.pop2

        case CZJUMP(success, failure, cond, kind) =>
          stack.pop

        case RETURN(kind) =>
          if (kind != UNIT)
            stack.pop;

        case THROW() =>
          stack.pop

        case DROP(kind) =>
          stack.pop

        case DUP(kind) =>
          stack.push(stack.head)

        case MONITOR_ENTER() =>
          stack.pop

        case MONITOR_EXIT() =>
          stack.pop

        case SCOPE_ENTER(_) | SCOPE_EXIT(_) =>
          ()

        case LOAD_EXCEPTION() =>
          stack.pop(stack.length)
          stack.push(typeLattice.Object)

        case _ =>
          dump
          abort("Unknown instruction: " + i)

      }
      out
    } // interpret


    class SimulatedStack {
      private var types: List[InferredType] = Nil
      private var depth = 0

      /** Remove and return the topmost element on the stack. If the
       *  stack is empty, return a reference to a negative index on the
       *  stack, meaning it refers to elements pushed by a predecessor block.
       */
      def pop: InferredType = types match {
        case head :: rest =>
          types = rest
          head
        case _ =>
          depth -= 1
          TypeOfStackPos(depth)
      }

      def pop2: (InferredType, InferredType) = {
        (pop, pop)
      }

      def push(t: InferredType) {
        depth += 1
        types = types ::: List(t)
      }

      def push(k: TypeKind) { push(Const(k)) }
    }

	abstract class InferredType {
      /** Return the type kind pointed by this inferred type. */
      def getKind(in: lattice.Elem): icodes.TypeKind = this match {
        case Const(k) =>
          k
        case TypeOfVar(l: icodes.Local) =>
          if (in.vars.isDefinedAt(l)) in.vars(l) else l.kind
        case TypeOfStackPos(n: Int) =>
          assert(in.stack.length >= n)
          in.stack(n)
      }
    }
	/** A type that does not depend on input to the transfer function. */
	case class Const(t: icodes.TypeKind) extends InferredType
	/** The type of a given local variable. */
	case class TypeOfVar(l: icodes.Local) extends InferredType
	/** The type found at a stack position. */
	case class TypeOfStackPos(n: Int) extends InferredType

	abstract class Gen
	case class Bind(l: icodes.Local, t: InferredType) extends Gen
	case class Push(t: InferredType) extends Gen

    /** A flow transfer function of a basic block. */
	class TransferFunction(consumed: Int, gens: List[Gen]) extends (lattice.Elem => lattice.Elem) {
	  def apply(in: lattice.Elem): lattice.Elem = {
        val out = lattice.IState(new VarBinding(in.vars), new TypeStack(in.stack))
        val bindings = out.vars
        val stack = out.stack

        out.stack.pop(consumed)
        for (g <- gens) g match {
          case Bind(l, t) =>
            out.vars += (l -> t.getKind(in))
          case Push(t) =>
            stack.push(t.getKind(in))
        }
        out
      }
	}
  }

  class Timer {
    var millis = 0L

    private var lastStart = 0L

    def reset {
      millis = 0L
    }

    def start {
      lastStart = System.currentTimeMillis
    }

    /** Stop the timer and return the number of milliseconds since the last
     * call to start. The 'millis' field is increased by the elapsed time.
     */
    def stop: Long = {
      val elapsed = System.currentTimeMillis - lastStart
      millis += elapsed
      elapsed
    }
  }
}
