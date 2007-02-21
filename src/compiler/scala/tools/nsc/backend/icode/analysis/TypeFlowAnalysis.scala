/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode.analysis

import scala.collection.mutable.{Map, HashMap}

/** A data-flow analysis on types, that works on ICode.
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
    val All    = icodes.REFERENCE(global.definitions.AllClass)

    def top    = Object
    def bottom = All

    def lub2(a: Elem, b: Elem) =
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

    def lub2(s1: TypeStack, s2: TypeStack) = {
      if (s1 eq bottom) s2
      else if (s2 eq bottom) s1
      else {
        if (s1.length != s2.length)
          throw new CheckerError("Incompatible stacks: " + s1 + " and " + s2);
        new TypeStack(List.map2(s1.types, s2.types) (icodes.lub))
      }
    }
  }

  /** A map which returns the bottom type for unfound elements */
  class VarBinding extends  HashMap[icodes.Local, icodes.TypeKind] {
    override def get(l: icodes.Local) = super.get(l) match {
      case Some(t) => Some(t);
      case None    => Some(typeLattice.bottom);
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
    type Elem = Pair[VarBinding, icodes.TypeStack]

    override val top    = new Pair(new VarBinding, typeStackLattice.top) {
      override def equals(that: Any) = (this eq that.asInstanceOf[AnyRef])
    }
    override val bottom = new Pair(new VarBinding, typeStackLattice.bottom) {
      override def equals(that: Any) = (this eq that.asInstanceOf[AnyRef])
    }

    def lub2(a: Elem, b: Elem) = {
      val Pair(env1, s1) = a
      val Pair(env2, s2) = b

      val resultingLocals = new VarBinding

      for (val binding1 <- env1.elements) {
        val tp2 = env2(binding1._1)
        resultingLocals += binding1._1 -> typeLattice.lub2(binding1._2, tp2)
      }

      for (val binding2 <- env2.elements; resultingLocals(binding2._1) eq typeLattice.bottom) {
        val tp1 = env1(binding2._1)
        resultingLocals += binding2._1 -> typeLattice.lub2(binding2._2, tp1)
      }

      Pair(resultingLocals, typeStackLattice.lub2(a._2, b._2))
    }
  }

  class MethodTFA extends DataFlowAnalysis[typeFlowLattice.type] {
    import icodes._
    import icodes.opcodes._

    type P = BasicBlock
    val lattice = typeFlowLattice

    val STRING = icodes.REFERENCE(TypeFlowAnalysis.this.global.definitions.StringClass)
    var method: IMethod = _

    /** Initialize the in/out maps for the analysis of the given method. */
    def init(m: icodes.IMethod): Unit = {
      this.method = m

      init {
        worklist += m.code.startBlock
        worklist ++= (m.exh map (.startBlock))
        m.code.blocks.foreach { b =>
          in(b)  = typeFlowLattice.bottom
          out(b) = typeFlowLattice.bottom
        }
        m.exh foreach { e =>
          val stack = new TypeStack
          stack.push(
            REFERENCE(if (e.cls eq NoSymbol) definitions.ObjectClass else e.cls))
          in(e.startBlock) = Pair(in(e.startBlock)._1, stack)
        }
      }
    }

    def this(m: icodes.IMethod) = {
      this()
      init(m)
    }

    def run = {
      forwardAnalysis(blockTransfer)
      if (settings.debug.value) {
        linearizer.linearize(method).foreach(b => if (b != method.code.startBlock)
          assert(visited.contains(b),
            "Block " + b + " in " + this.method + " has input equal to bottom -- not visited? .." + visited));
      }
    }

    def blockTransfer(b: BasicBlock, in: lattice.Elem): lattice.Elem =
      b.toList.foldLeft(in)(interpret)

    /** Abstract interpretation for one instruction. */
    def interpret(in: typeFlowLattice.Elem, i: Instruction): typeFlowLattice.Elem = {
      val out = Pair(new VarBinding(in._1), new TypeStack(in._2))
      val bindings = out._1
      val stack = out._2

/*      if (settings.debug.value) {
        Console.println("Stack: " + stack);
        Console.println(i);
      } */
      i match {

        case THIS(clasz) =>
          stack push toTypeKind(clasz.tpe)

        case CONSTANT(const) =>
          stack push toTypeKind(const.tpe)

        case LOAD_ARRAY_ITEM(kind) =>
          val Pair(INT, ARRAY(elem)) = stack.pop2
          stack.push(elem)

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
          bindings += local -> t

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
              stack push kind

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

        case CREATE_ARRAY(elem) =>
          stack.pop
          stack.push(ARRAY(elem))

        case IS_INSTANCE(tpe) =>
          stack.pop
          stack.push(BOOL)

        case CHECK_CAST(tpe) =>
          stack.pop
          stack.push(tpe)

        case SWITCH(tags, labels) =>
          stack.pop

        case JUMP(where) =>
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

        case _ =>
          dump
          abort("Unknown instruction: " + i)

      }
      out
    } // interpret
  }
}
