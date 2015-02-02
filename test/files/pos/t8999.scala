object Types {

  abstract sealed class Type

  case object AnyType extends Type

  case object NothingType extends Type

  case object UndefType extends Type

  case object BooleanType extends Type

  case object IntType extends Type

  case object LongType extends Type

  case object FloatType extends Type

  case object DoubleType extends Type

  case object StringType extends Type

  case object NullType extends Type

  sealed abstract class ReferenceType extends Type

  final case class ClassType(className: String) extends ReferenceType

  final case class ArrayType(baseClassName: String, dimensions: Int) extends ReferenceType

  final case class RecordType(fields: List[RecordType.Field]) extends Type

  object RecordType {
    final case class Field(name: String, originalName: Option[String],
        tpe: Type, mutable: Boolean)
  }

  case object NoType extends Type

}


sealed abstract class ClassKind

object ClassKind {

  case object Class extends ClassKind

  case object ModuleClass extends ClassKind

  case object Interface extends ClassKind

  case object RawJSType extends ClassKind

  case object HijackedClass extends ClassKind

  case object TraitImpl extends ClassKind

}

object Trees {

  import Types._

  abstract sealed class Tree

  case object EmptyTree extends Tree

  sealed trait PropertyName
  case class Ident(name: String, originalName: Option[String]) extends PropertyName
  object Ident {
    def apply(name: String): Ident =
      new Ident(name, Some(name))
  }

  case class VarDef(name: Ident, vtpe: Type, mutable: Boolean, rhs: Tree) extends Tree

  case class ParamDef(name: Ident, ptpe: Type, mutable: Boolean) extends Tree

  case class Skip() extends Tree

  class Block private(val stats: List[Tree]) extends Tree

  object Block {
    def unapply(block: Block): Some[List[Tree]] = Some(block.stats)
  }

  case class Labeled(label: Ident, tpe: Type, body: Tree) extends Tree

  case class Assign(lhs: Tree, rhs: Tree) extends Tree

  case class Return(expr: Tree, label: Option[Ident] = None) extends Tree

  case class If(cond: Tree, thenp: Tree, elsep: Tree) extends Tree

  case class While(cond: Tree, body: Tree, label: Option[Ident] = None) extends Tree

  case class DoWhile(body: Tree, cond: Tree, label: Option[Ident] = None) extends Tree

  case class Try(block: Tree, errVar: Ident, handler: Tree, finalizer: Tree) extends Tree

  case class Throw(expr: Tree) extends Tree

  case class Continue(label: Option[Ident] = None) extends Tree

  case class Match(selector: Tree, cases: List[(List[Literal], Tree)], default: Tree) extends Tree

  case class Debugger() extends Tree

  case class New(cls: ClassType, ctor: Ident, args: List[Tree]) extends Tree

  case class LoadModule(cls: ClassType) extends Tree

  case class StoreModule(cls: ClassType, value: Tree) extends Tree

  case class Select(qualifier: Tree, item: Ident, mutable: Boolean) extends Tree

  case class Apply(receiver: Tree, method: Ident, args: List[Tree]) extends Tree

  case class StaticApply(receiver: Tree, cls: ClassType, method: Ident, args: List[Tree]) extends Tree

  case class TraitImplApply(impl: ClassType, method: Ident, args: List[Tree]) extends Tree

  case class UnaryOp(op: Int, lhs: Tree) extends Tree

  case class BinaryOp(op: Int, lhs: Tree, rhs: Tree) extends Tree

  case class NewArray(tpe: ArrayType, lengths: List[Tree]) extends Tree

  case class ArrayValue(tpe: ArrayType, elems: List[Tree]) extends Tree

  case class ArrayLength(array: Tree) extends Tree

  case class ArraySelect(array: Tree, index: Tree) extends Tree

  case class RecordValue(tpe: RecordType, elems: List[Tree]) extends Tree

  case class IsInstanceOf(expr: Tree, cls: ReferenceType) extends Tree

  case class AsInstanceOf(expr: Tree, cls: ReferenceType) extends Tree

  case class Unbox(expr: Tree, charCode: Char) extends Tree

  case class GetClass(expr: Tree) extends Tree

  case class CallHelper(helper: String, args: List[Tree]) extends Tree

  case class JSNew(ctor: Tree, args: List[Tree]) extends Tree

  case class JSDotSelect(qualifier: Tree, item: Ident) extends Tree

  case class JSBracketSelect(qualifier: Tree, item: Tree) extends Tree

  case class JSFunctionApply(fun: Tree, args: List[Tree]) extends Tree

  case class JSDotMethodApply(receiver: Tree, method: Ident, args: List[Tree]) extends Tree

  case class JSBracketMethodApply(receiver: Tree, method: Tree, args: List[Tree]) extends Tree

  case class JSDelete(prop: Tree) extends Tree

  case class JSUnaryOp(op: String, lhs: Tree) extends Tree

  case class JSBinaryOp(op: String, lhs: Tree, rhs: Tree) extends Tree

  case class JSArrayConstr(items: List[Tree]) extends Tree

  case class JSObjectConstr(fields: List[(PropertyName, Tree)]) extends Tree

  case class JSEnvInfo() extends Tree

  sealed trait Literal extends Tree

  case class Undefined() extends Literal

  case class UndefinedParam() extends Literal

  case class Null() extends Literal

  case class BooleanLiteral(value: Boolean) extends Literal

  case class IntLiteral(value: Int) extends Literal

  case class LongLiteral(value: Long) extends Literal

  case class FloatLiteral(value: Float) extends Literal

  case class DoubleLiteral(value: Double) extends Literal

  case class StringLiteral(value: String) extends Literal with PropertyName

  case class ClassOf(cls: ReferenceType) extends Literal

  case class VarRef(ident: Ident, mutable: Boolean) extends Tree

  case class This() extends Tree

  case class Closure(captureParams: List[ParamDef], params: List[ParamDef],
                     body: Tree, captureValues: List[Tree]) extends Tree

  case class ClassDef(name: Ident, kind: ClassKind, parent: Option[Ident], ancestors: List[Ident], defs: List[Tree]) extends Tree

  case class MethodDef(name: PropertyName, args: List[ParamDef], resultType: Type, body: Tree) extends Tree

  case class PropertyDef(name: PropertyName, getterBody: Tree, setterArg: ParamDef, setterBody: Tree) extends Tree

  case class ConstructorExportDef(name: String, args: List[ParamDef], body: Tree) extends Tree

  case class ModuleExportDef(fullName: String) extends Tree

  final class TreeHash(val treeHash: Array[Byte], val posHash: Array[Byte])
}

object Main {
  import Trees._
  import Types._

  private def transform(tree: Tree) = {
    val ObjectClass = "O"
    tree match {
      case VarDef(_, _, _, rhs)                                             =>
      case tree: Block                                                      =>
      case Labeled(ident@Ident(label, _), tpe, body)                        =>
      case Assign(lhs, rhs)                                                 =>
      case Return(expr, optLabel)                                           =>
      case If(cond, thenp, elsep)                                           =>
      case While(cond, body, optLabel)                                      =>
      case DoWhile(body, cond, None)                                        =>
      case Try(block, errVar, EmptyTree, finalizer)                         =>
      case Try(block, errVar@Ident(name, originalName), handler, finalizer) =>
      case Throw(expr)                                                      =>
      case Continue(optLabel)                                               =>
      case Match(selector, cases, default)                                  =>
      case New(cls, ctor, args)                                             =>
      case StoreModule(cls, value)                                          =>
      case tree: Select                                                     =>
      case tree: Apply                                                      =>
      case tree: StaticApply                                                =>
      case tree: TraitImplApply                                             =>
      case tree@UnaryOp(_, arg)                                             =>
      case tree@BinaryOp(op, lhs, rhs)                                      =>
      case NewArray(tpe, lengths)                                           =>
      case ArrayValue(tpe, elems)                                           =>
      case ArrayLength(array)                                               =>
      case ArraySelect(array, index)                                        =>
      case RecordValue(tpe, elems)                                          =>
      case IsInstanceOf(expr, ClassType(ObjectClass))                       =>
      case IsInstanceOf(expr, tpe)                                          =>
      case AsInstanceOf(expr, ClassType(ObjectClass))                       =>
      case AsInstanceOf(expr, cls)                                          =>
      case Unbox(arg, charCode)                                             =>
      case GetClass(expr)                                                   =>
      case JSNew(ctor, args)                                                =>
      case JSDotSelect(qualifier, item)                                     =>
      case JSBracketSelect(qualifier, item)                                 =>
      case tree: JSFunctionApply                                            =>
      case JSDotMethodApply(receiver, method, args)                         =>
      case JSBracketMethodApply(receiver, method, args)                     =>
      case JSDelete(JSDotSelect(obj, prop))                                 =>
      case JSDelete(JSBracketSelect(obj, prop))                             =>
      case JSUnaryOp(op, lhs)                                               =>
      case JSBinaryOp(op, lhs, rhs)                                         =>
      case JSArrayConstr(items)                                             =>
      case JSObjectConstr(fields)                                           =>
      case _: VarRef | _: This                                              =>
      case Closure(captureParams, params, body, captureValues)              =>
      case _: Skip | _: Debugger | _: LoadModule |
           _: JSEnvInfo | _: Literal | EmptyTree                            =>
    }
  }
}