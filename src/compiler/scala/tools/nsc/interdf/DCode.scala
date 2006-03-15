package scala.tools.nsc.interdf

/** A variation of ICode that uses direct variable access
  * instead of a stack. It is not needed for compilation, but
  * it is a convenient representation for analysis.
  *
  * Note that instructions are grouped into basic blocks.
  * Control flow between blocks is represented at the
  * block level, not in instructions -- not even
  * control instructions.
  */
abstract class DCode
extends Object
with ICodeToDCode
{
  val compiler: Global
  import compiler._

  /** A variable local to a Code instance; it corresponds to a stack position. */
  case class StackVar(id: int) {
    override def toString = "sv$" + id
  }

  /** A counter for generating fresh StackVar's */
  private var svctr = 0

  /** Generate a fresh StackVar. */
  def genStackVar = {
    svctr = svctr + 1
    new StackVar(svctr)
  }

  /** One instruction to execute. */
  abstract class Instruction
  case class StrTrip(str: String) extends Instruction // XXX kill this
  object opcodes {
    import compiler.icodes.{Primitive, TypeKind, TestOp, Local}
    import compiler.icodes.opcodes.InvokeStyle


    /* standard pattern match:
         case THIS(lhs, clasz)  =>
         case CONSTANT(lhs, const)  =>
         CASE VAR(lhs, rhs) =>
         case LOAD_ARRAY_ITEM(lhs, ary, idx, kind)  =>
         case LOAD_LOCAL(lhs, local, isArg)  =>
         case LOAD_FIELD(lhs, from, field, isStatic)  =>
         case LOAD_MODULE(lhs, module)  =>
         case STORE_ARRAY_ITEM(ary, idx, obj, kind)  =>
         case STORE_LOCAL(local, obj, isArg)  =>
         case STORE_FIELD(into, field, obj, isStatic)  =>
         case CALL_PRIMITIVE(lhs, primitive, args)  =>
         case CALL_METHOD(lhs, rcvr, method, args, style)  =>
         case NEW(lhs, kind)  =>
         case CREATE_ARRAY(lhs, elem, size)  =>
         case IS_INSTANCE(lhs, rhs, tpe)  =>
         case CHECK_CAST(lhs, rhs, tpe)  =>
         case SWITCH(obj, tags[List[int]])  =>
         case JUMP()  =>
         case CJUMP(left, cond, right, kind)  =>
         case CZJUMP(obj, cond, kind)  =>
         case RETURN(obj, kind)  =>
         case RETURN_VOID(kind) =>
         case THROW(exc)  =>
         case MONITOR_ENTER(obj)  =>
         case MONITOR_EXIT(obj)  =>
         case NOP()  =>
     */


    case class THIS(lhs: StackVar, clasz: Symbol) extends Instruction
    case class CONSTANT(lhs: StackVar, const: Constant) extends Instruction
    case class VAR(lhs: StackVar, rhs: StackVar) extends Instruction
    case class LOAD_ARRAY_ITEM(lhs: StackVar, ary: StackVar, idx: StackVar, kind: TypeKind) extends Instruction
    case class LOAD_LOCAL(lhs: StackVar, local: Local, isArg: Boolean) extends Instruction
    case class LOAD_FIELD(lhs: StackVar, from: StackVar, field: Symbol, isStatic: Boolean) extends Instruction
    case class LOAD_MODULE(lhs: StackVar, module: Symbol) extends Instruction
    case class STORE_ARRAY_ITEM(ary: StackVar, idx: StackVar, obj: StackVar, kind: TypeKind) extends Instruction
    case class STORE_LOCAL(local: Local, obj: StackVar, isArg: Boolean) extends Instruction
    case class STORE_FIELD(into: StackVar, field: Symbol, obj: StackVar, isStatic: Boolean) extends Instruction
    case class CALL_PRIMITIVE(lhs: StackVar, primitive: Primitive, args: List[StackVar]) extends Instruction
    case class CALL_METHOD(lhs: StackVar, rcvr: Option[StackVar], method: Symbol, args: List[StackVar], style: InvokeStyle) extends Instruction
    case class NEW(lhs: StackVar, kind: TypeKind) extends Instruction
    case class CREATE_ARRAY(lhs: StackVar, elem: TypeKind, size: StackVar) extends Instruction
    case class IS_INSTANCE(lhs: StackVar, rhs: StackVar, tpe: TypeKind) extends Instruction
    case class CHECK_CAST(lhs: StackVar, rhs: StackVar, tpe: TypeKind) extends Instruction
    case class SWITCH(obj: StackVar, tags: List[List[int]]) extends Instruction
    case class JUMP() extends Instruction
    case class CJUMP(left: StackVar, cond: TestOp, right: StackVar, kind: TypeKind) extends Instruction
    case class CZJUMP(obj: StackVar, cond: TestOp, kind: TypeKind) extends Instruction
    case class RETURN(obj: StackVar, kind: TypeKind) extends Instruction
    case class RETURN_VOID(kind: TypeKind) extends Instruction
    case class THROW(exc: StackVar) extends Instruction
    case class MONITOR_ENTER(obj: StackVar) extends Instruction
    case class MONITOR_EXIT(obj: StackVar) extends Instruction
    case class NOP() extends Instruction
  }

  class BasicBlock(
      var instrs: List[Instruction],
      var next: List[BasicBlock])
  {
    def this() = this(Nil, Nil)
  }

  class Code(val blocks: List[BasicBlock])
  {
    def start = blocks.head
  }
}

