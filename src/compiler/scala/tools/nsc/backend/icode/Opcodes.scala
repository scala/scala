/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */



package scala.tools.nsc
package backend
package icode

import scala.tools.nsc.ast._
import scala.reflect.internal.util.{Position,NoPosition}

/*
  A pattern match

  // locals
    case THIS(clasz) =>
    case STORE_THIS(kind) =>
    case LOAD_LOCAL(local) =>
    case STORE_LOCAL(local) =>
    case SCOPE_ENTER(lv) =>
    case SCOPE_EXIT(lv) =>
  // stack
    case LOAD_MODULE(module) =>
    case LOAD_EXCEPTION(clasz) =>
    case DROP(kind) =>
    case DUP(kind) =>
  // constants
    case CONSTANT(const) =>
  // arithlogic
    case CALL_PRIMITIVE(primitive) =>
  // casts
    case IS_INSTANCE(tpe) =>
    case CHECK_CAST(tpe) =>
  // objs
    case NEW(kind) =>
    case MONITOR_ENTER() =>
    case MONITOR_EXIT() =>
    case BOX(boxType) =>
    case UNBOX(tpe) =>
  // flds
    case LOAD_FIELD(field, isStatic) =>
    case STORE_FIELD(field, isStatic) =>
  // mthds
    case CALL_METHOD(method, style) =>
  // arrays
    case LOAD_ARRAY_ITEM(kind) =>
    case STORE_ARRAY_ITEM(kind) =>
    case CREATE_ARRAY(elem, dims) =>
  // jumps
    case SWITCH(tags, labels) =>
    case JUMP(whereto) =>
    case CJUMP(success, failure, cond, kind) =>
    case CZJUMP(success, failure, cond, kind) =>
  // ret
    case RETURN(kind) =>
    case THROW(clasz) =>
*/


/**
 * The ICode intermediate representation. It is a stack-based
 * representation, very close to the JVM and .NET. It uses the
 * erased types of Scala and references Symbols to refer named entities
 * in the source files.
 */
trait Opcodes { self: ICodes =>
  import global.{Symbol, NoSymbol, Type, Name, Constant};

  // categories of ICode instructions
  final val localsCat =  1
  final val stackCat  =  2
  final val constCat  =  3
  final val arilogCat =  4
  final val castsCat  =  5
  final val objsCat   =  6
  final val fldsCat   =  7
  final val mthdsCat  =  8
  final val arraysCat =  9
  final val jumpsCat  = 10
  final val retCat    = 11

  /** This class represents an instruction of the intermediate code.
   *  Each case subclass will represent a specific operation.
   */
  abstract class Instruction extends Cloneable {

    def category: Int = 0 // undefined

    /** This abstract method returns the number of used elements on the stack */
    def consumed : Int = 0

    /** This abstract method returns the number of produced elements on the stack */
    def produced : Int = 0

    /** This instruction consumes these types from the top of the stack, the first
     *  element in the list is the deepest element on the stack.
     */
    def consumedTypes: List[TypeKind] = Nil

    /** This instruction produces these types on top of the stack. */
    def producedTypes: List[TypeKind] = Nil

    /** This method returns the difference of size of the stack when the instruction is used */
    def difference = produced-consumed

    /** The corresponding position in the source file */
    private var _pos: Position = NoPosition

    def pos: Position = _pos

    /** Used by dead code elimination. */
    var useful: Boolean = false

    def setPos(p: Position): this.type = {
      _pos = p
      this
    }

    /** Clone this instruction. */
    override def clone: Instruction =
      super.clone.asInstanceOf[Instruction]
  }

  object opcodes {

    def mayThrow(i: Instruction): Boolean = i match {
      case LOAD_LOCAL(_) | STORE_LOCAL(_) | CONSTANT(_) | THIS(_) | CZJUMP(_, _, _, _)
              | DROP(_) | DUP(_) | RETURN(_) | LOAD_EXCEPTION(_) | JUMP(_) | CJUMP(_, _, _, _) => false
      case _ => true
    }

    /** Loads "this" on top of the stack.
     * Stack: ...
     *    ->: ...:ref
     */
    case class THIS(clasz: Symbol) extends Instruction {
      /** Returns a string representation of this constant */
      override def toString = "THIS(" + clasz.name + ")"

      override def consumed = 0
      override def produced = 1

      override def producedTypes = List(REFERENCE(clasz))

      override def category = localsCat
    }

    /** Loads a constant on the stack.
     * Stack: ...
     *    ->: ...:constant
     */
    case class CONSTANT(constant: Constant) extends Instruction {
      override def toString = "CONSTANT(" + constant.escapedStringValue + ")"
      override def consumed = 0
      override def produced = 1

      override def producedTypes = List(toTypeKind(constant.tpe))

      override def category = constCat
    }

    /** Loads an element of an array. The array and the index should
     * be on top of the stack.
     * Stack: ...:array[a](Ref):index(Int)
     *    ->: ...:element(a)
     */
    case class LOAD_ARRAY_ITEM(kind: TypeKind) extends Instruction {
      override def consumed = 2
      override def produced = 1

      override def consumedTypes = List(ARRAY(kind), INT)
      override def producedTypes = List(kind)

      override def category = arraysCat
    }

    /** Load a local variable on the stack. It can be a method argument.
     * Stack: ...
     *    ->: ...:value
     */
    case class LOAD_LOCAL(local: Local) extends Instruction {
      override def consumed = 0
      override def produced = 1

      override def producedTypes = List(local.kind)

      override def category = localsCat
    }

    /** Load a field on the stack. The object to which it refers should be
     * on the stack.
     * Stack: ...:ref       (assuming isStatic = false)
     *    ->: ...:value
     */
    case class LOAD_FIELD(field: Symbol, isStatic: Boolean) extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String =
        "LOAD_FIELD " + (if (isStatic) field.fullName else field.toString());

      override def consumed = if (isStatic) 0 else 1
      override def produced = 1

      override def consumedTypes = if (isStatic) Nil else List(REFERENCE(field.owner));
      override def producedTypes = List(toTypeKind(field.tpe));

      // more precise information about how to load this field
      // see #4283
      var hostClass: Symbol = field.owner
      def setHostClass(cls: Symbol): this.type = { hostClass = cls; this }

      override def category = fldsCat
    }

    case class LOAD_MODULE(module: Symbol) extends Instruction {
      assert(module != NoSymbol, "Invalid module symbol")
      /** Returns a string representation of this instruction */
      override def toString(): String = "LOAD_MODULE " + module

      override def consumed = 0
      override def produced = 1

      override def producedTypes = List(REFERENCE(module))

      override def category = stackCat
    }

    /** Store a value into an array at a specified index.
     * Stack: ...:array[a](Ref):index(Int):value(a)
     *    ->: ...
     */
    case class STORE_ARRAY_ITEM(kind: TypeKind) extends Instruction {
      override def consumed = 3
      override def produced = 0

      override def consumedTypes = List(ARRAY(kind), INT, kind)

      override def category = arraysCat
    }

    /** Store a value into a local variable. It can be an argument.
     * Stack: ...:value
     *    ->: ...
     */
    case class STORE_LOCAL(local: Local) extends Instruction {
      override def consumed = 1
      override def produced = 0

      override def consumedTypes = List(local.kind)

      override def category = localsCat
    }

    /** Store a value into a field.
     * Stack: ...:ref:value   (assuming isStatic=false)
     *    ->: ...
     */
    case class STORE_FIELD(field: Symbol, isStatic: Boolean) extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String =
        "STORE_FIELD "+field + (if (isStatic) " (static)" else " (dynamic)");

      override def consumed = if(isStatic) 1 else 2;
      override def produced = 0;

      override def consumedTypes =
        if (isStatic)
          List(toTypeKind(field.tpe))
        else
          List(REFERENCE(field.owner), toTypeKind(field.tpe));

      override def category = fldsCat
    }

    /** Store a value into the 'this' pointer.
       * Stack: ...:ref
       *    ->: ...
       */
    case class STORE_THIS(kind: TypeKind) extends Instruction {
      override def consumed = 1
      override def produced = 0
      override def consumedTypes = List(kind)
      override def category = localsCat
    }

    /** Call a primitive function.
     * Stack: ...:arg1:arg2:...:argn
     *    ->: ...:result
     */
    case class CALL_PRIMITIVE(primitive: Primitive) extends Instruction {
      override def consumed = primitive match {
        case Negation(_)       => 1
        case Test(_,_, true)   => 1
        case Test(_,_, false)  => 2
        case Comparison(_,_)   => 2
        case Arithmetic(NOT,_) => 1
        case Arithmetic(_,_)   => 2
        case Logical(_,_)      => 2
        case Shift(_,_)        => 2
        case Conversion(_,_)   => 1
        case ArrayLength(_)    => 1
        case StringConcat(_)   => 2
        case StartConcat       => 0
        case EndConcat         => 1
      }
      override def produced = 1

      override def consumedTypes = primitive match {
        case Negation(kind)        => List(kind)
        case Test(_, kind, true)   => List(kind)
        case Test(_, kind, false)  => List(kind, kind)
        case Comparison(_, kind)   => List(kind, kind)
        case Arithmetic(NOT, kind) => List(kind)
        case Arithmetic(_, kind)   => List(kind, kind)
        case Logical(_, kind)      => List(kind, kind)
        case Shift(_, kind)        => List(kind, INT)
        case Conversion(from, _)   => List(from)
        case ArrayLength(kind)     => List(ARRAY(kind))
        case StringConcat(kind)    => List(ConcatClass, kind)
        case StartConcat           => Nil
        case EndConcat             => List(ConcatClass)
      }

      override def producedTypes = primitive match {
        case Negation(kind)      => List(kind)
        case Test(_, _, true)    => List(BOOL)
        case Test(_, _, false)   => List(BOOL)
        case Comparison(_, _)    => List(INT)
        case Arithmetic(_, kind) => List(kind)
        case Logical(_, kind)    => List(kind)
        case Shift(_, kind)      => List(kind)
        case Conversion(_, to)   => List(to)
        case ArrayLength(_)      => List(INT)
        case StringConcat(_)     => List(ConcatClass)
        case StartConcat         => List(ConcatClass)
        case EndConcat           => List(REFERENCE(global.definitions.StringClass))
      }

      override def category = arilogCat
   }

    /** This class represents a CALL_METHOD instruction
     * STYLE: dynamic / static(StaticInstance)
     * Stack: ...:ref:arg1:arg2:...:argn
     *    ->: ...:result
     *
     * STYLE: static(StaticClass)
     * Stack: ...:arg1:arg2:...:argn
     *    ->: ...:result
     *
     */
    case class CALL_METHOD(method: Symbol, style: InvokeStyle) extends Instruction with ReferenceEquality {
      def toShortString =
        "CALL_METHOD " + method.name +" ("+style+")"

      /** Returns a string representation of this instruction */
      override def toString(): String =
        "CALL_METHOD " + method.fullName +" ("+style+")"

      var hostClass: Symbol = method.owner
      def setHostClass(cls: Symbol): this.type = { hostClass = cls; this }

      /** This is specifically for preserving the target native Array type long
       *  enough that clone() can generate the right call.
       */
      var targetTypeKind: TypeKind = UNIT // the default should never be used, so UNIT should fail fast.
      def setTargetTypeKind(tk: TypeKind) = targetTypeKind = tk

      private def params = method.info.paramTypes
      private def consumesInstance = style match {
        case Static(false)  => 0
        case _              => 1
      }

      override def consumed = params.length + consumesInstance
      override def consumedTypes = {
        val args = params map toTypeKind
        if (consumesInstance > 0) ObjectReference :: args
        else args
      }

      override def produced =
        if (producedType == UNIT || method.isConstructor) 0
        else 1

      private def producedType: TypeKind = toTypeKind(method.info.resultType)
      override def producedTypes =
        if (produced == 0) Nil
        else List(producedType)

      /** object identity is equality for CALL_METHODs. Needed for
       *  being able to store such instructions into maps, when more
       *  than one CALL_METHOD to the same method might exist.
       */

      override def category = mthdsCat
    }

    case class BOX(boxType: TypeKind) extends Instruction {
      assert(boxType.isValueType && (boxType ne UNIT)) // documentation
      override def toString(): String = "BOX " + boxType
      override def consumed = 1
      override def consumedTypes = boxType :: Nil
      override def produced = 1
      override def category = objsCat
    }

    case class UNBOX(boxType: TypeKind) extends Instruction {
      assert(boxType.isValueType && (boxType ne UNIT)) // documentation
      override def toString(): String = "UNBOX " + boxType
      override def consumed = 1
      override def consumedTypes = ObjectReference :: Nil
      override def produced = 1
      override def category = objsCat
    }

    /** Create a new instance of a class through the specified constructor
     * Stack: ...:arg1:arg2:...:argn
     *    ->: ...:ref
     */
    case class NEW(kind: REFERENCE) extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String = "NEW "+ kind;

      override def consumed = 0;
      override def produced = 1;

      /** The corresponding constructor call. */
      var init: CALL_METHOD = _

      override def category = objsCat
    }


    /** This class represents a CREATE_ARRAY instruction
     * Stack: ...:size_1:size_2:..:size_n
     *    ->: ...:arrayref
     */
    case class CREATE_ARRAY(elem: TypeKind, dims: Int) extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String ="CREATE_ARRAY "+elem + " x " + dims;

      override def consumed = dims;
      override def consumedTypes = List.fill(dims)(INT)
      override def produced = 1;

      override def category = arraysCat
    }

    /** This class represents a IS_INSTANCE instruction
     * Stack: ...:ref
     *    ->: ...:result(boolean)
     */
    case class IS_INSTANCE(typ: TypeKind) extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String ="IS_INSTANCE "+typ

      override def consumed = 1
      override def consumedTypes = ObjectReference :: Nil
      override def produced = 1

      override def category = castsCat
    }

    /** This class represents a CHECK_CAST instruction
     * Stack: ...:ref(oldtype)
     *    ->: ...:ref(typ <=: oldtype)
     */
    case class CHECK_CAST(typ: TypeKind) extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String ="CHECK_CAST "+typ

      override def consumed = 1
      override def produced = 1
      override val consumedTypes = List(ObjectReference)
      override def producedTypes = List(typ)

      override def category = castsCat
    }

    /** This class represents a SWITCH instruction
     * Stack: ...:index(int)
     *    ->: ...:
     *
     * The tags array contains one entry per label, each entry consisting of
     * an array of ints, any of which will trigger the jump to the corresponding label.
     * labels should contain an extra label, which is the 'default' jump.
     */
    case class SWITCH(tags: List[List[Int]], labels: List[BasicBlock]) extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String ="SWITCH ..."

      override def consumed = 1
      override def produced = 0

      override val consumedTypes = List(INT)

      def flatTagsCount: Int = { var acc = 0; var rest = tags; while(rest.nonEmpty) { acc += rest.head.length; rest = rest.tail }; acc } // a one-liner

      override def category = jumpsCat
    }

    /** This class represents a JUMP instruction
     * Stack: ...
     *    ->: ...
     */
    case class JUMP(whereto: BasicBlock) extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String ="JUMP "+whereto.label

      override def consumed = 0
      override def produced = 0

      override def category = jumpsCat
    }

    /** This class represents a CJUMP instruction
     * It compares the two values on the stack with the 'cond' test operator
     * Stack: ...:value1:value2
     *    ->: ...
     */
    case class CJUMP(successBlock: BasicBlock,
		     failureBlock: BasicBlock,
		     cond: TestOp,
                     kind: TypeKind) extends Instruction
    {

      /** Returns a string representation of this instruction */
      override def toString(): String = (
        "CJUMP (" + kind + ")" +
        cond + " ? "+successBlock.label+" : "+failureBlock.label
      );

      override def consumed = 2
      override def produced = 0

      override val consumedTypes = List(kind, kind)

      override def category = jumpsCat
    }

    /** This class represents a CZJUMP instruction
     * It compares the one value on the stack and zero with the 'cond' test operator
     * Stack: ...:value:
     *    ->: ...
     */
    case class CZJUMP(successBlock: BasicBlock,
                      failureBlock: BasicBlock,
                      cond: TestOp,
                      kind: TypeKind) extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String = (
        "CZJUMP (" + kind + ")" +
        cond + " ? "+successBlock.label+" : "+failureBlock.label
      );

      override def consumed = 1
      override def produced = 0

      override val consumedTypes = List(kind)

      override def category = jumpsCat
    }


    /** This class represents a RETURN instruction
     * Stack: ...
     *    ->: ...
     */
    case class RETURN(kind: TypeKind) extends Instruction {
      override def consumed = if (kind == UNIT) 0 else 1
      override def produced = 0

      // TODO override val consumedTypes = List(kind)

      override def category = retCat
    }

    /** This class represents a THROW instruction
     * Stack: ...:Throwable(Ref)
     *    ->: ...:
     */
    case class THROW(clasz: Symbol) extends Instruction {
      /** PP to ID: We discussed parameterizing LOAD_EXCEPTION but
       *  not THROW, which came about organically.  It seems like the
       *  right thing, but can you confirm?
       */
      override def toString = "THROW(" + clasz.name + ")"

      override def consumed = 1
      override def produced = 0

      override def category = retCat
    }

    /** This class represents a DROP instruction
     * Stack: ...:something
     *    ->: ...
     */
    case class DROP (typ: TypeKind) extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String ="DROP "+typ

      override def consumed = 1
      override def produced = 0

      override def category = stackCat
    }

    /** This class represents a DUP instruction
     * Stack: ...:something
     *    ->: ...:something:something
     */
    case class DUP (typ: TypeKind) extends Instruction {
      override def consumed = 1
      override def produced = 2
      override def category = stackCat
    }

    /** This class represents a MONITOR_ENTER instruction
     * Stack: ...:object(ref)
     *    ->: ...:
     */
    case class MONITOR_ENTER() extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String ="MONITOR_ENTER"

      override def consumed = 1
      override def produced = 0

      override def category = objsCat
    }

    /** This class represents a MONITOR_EXIT instruction
     * Stack: ...:object(ref)
     *    ->: ...:
     */
    case class MONITOR_EXIT() extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String ="MONITOR_EXIT";

      override def consumed = 1;
      override def produced = 0;

      override def category = objsCat
    }

    /** A local variable becomes visible at this point in code.
     *  Used only for generating precise local variable tables as
     *  debugging information.
     */
    case class SCOPE_ENTER(lv: Local) extends Instruction {
      override def toString(): String = "SCOPE_ENTER " + lv
      override def consumed = 0
      override def produced = 0
      override def category = localsCat
    }

    /** A local variable leaves its scope at this point in code.
     *  Used only for generating precise local variable tables as
     *  debugging information.
     */
    case class SCOPE_EXIT(lv: Local) extends Instruction {
      override def toString(): String = "SCOPE_EXIT " + lv
      override def consumed = 0
      override def produced = 0
      override def category = localsCat
    }

    /** Fake instruction. It designates the VM who pushes an exception
     *  on top of the /empty/ stack at the beginning of each exception handler.
     *  Note: Unlike other instructions, it consumes all elements on the stack!
     *        then pushes one exception instance.
     */
    case class LOAD_EXCEPTION(clasz: Symbol) extends Instruction {
      override def consumed = sys.error("LOAD_EXCEPTION does clean the whole stack, no idea how many things it consumes!")
      override def produced = 1
      override def producedTypes = REFERENCE(clasz) :: Nil
      override def category = stackCat
    }

    /** This class represents a method invocation style. */
    sealed abstract class InvokeStyle {
      /** Is this a dynamic method call? */
      def isDynamic: Boolean = false

      /** Is this a static method call? */
      def isStatic: Boolean = false

      def isSuper: Boolean = false

      /** Is this an instance method call? */
      def hasInstance: Boolean = true

      /** Returns a string representation of this style. */
      override def toString(): String
    }

    /** Virtual calls.
     *  On JVM, translated to either `invokeinterface` or `invokevirtual`.
     */
    case object Dynamic extends InvokeStyle {
      override def isDynamic = true
      override def toString(): String = "dynamic"
    }

    /**
     * Special invoke:
     *   Static(true)  is used for calls to private members, ie `invokespecial` on JVM.
     *   Static(false) is used for calls to class-level instance-less static methods, ie `invokestatic` on JVM.
     */
    case class Static(onInstance: Boolean) extends InvokeStyle {
      override def isStatic    = true
      override def hasInstance = onInstance
      override def toString(): String = {
        if(onInstance) "static-instance"
        else           "static-class"
      }
    }

    /** Call through super[mix].
     *  On JVM, translated to `invokespecial`.
     */
    case class SuperCall(mix: Name) extends InvokeStyle {
      override def isSuper = true
      override def toString(): String = { "super(" + mix + ")" }
    }


    // CLR backend

    case class CIL_LOAD_LOCAL_ADDRESS(local: Local) extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String = "CIL_LOAD_LOCAL_ADDRESS "+local  //+isArgument?" (argument)":"";

      override def consumed = 0
      override def produced = 1

      override def producedTypes = List(msil_mgdptr(local.kind))

      override def category = localsCat
  }

    case class CIL_LOAD_FIELD_ADDRESS(field: Symbol, isStatic: Boolean) extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String =
        "CIL_LOAD_FIELD_ADDRESS " + (if (isStatic) field.fullName else field.toString)

      override def consumed = if (isStatic) 0 else 1
      override def produced = 1

      override def consumedTypes = if (isStatic) Nil else List(REFERENCE(field.owner));
      override def producedTypes = List(msil_mgdptr(REFERENCE(field.owner)));

      override def category = fldsCat
}

    case class CIL_LOAD_ARRAY_ITEM_ADDRESS(kind: TypeKind) extends Instruction {
      /** Returns a string representation of this instruction */
      override def toString(): String = "CIL_LOAD_ARRAY_ITEM_ADDRESS (" + kind + ")"

      override def consumed = 2
      override def produced = 1

      override def consumedTypes = List(ARRAY(kind), INT)
      override def producedTypes = List(msil_mgdptr(kind))

      override def category = arraysCat
    }

    case class CIL_UNBOX(valueType: TypeKind) extends Instruction {
      override def toString(): String = "CIL_UNBOX " + valueType
      override def consumed = 1
      override def consumedTypes = ObjectReference :: Nil // actually consumes a 'boxed valueType'
      override def produced = 1
      override def producedTypes = List(msil_mgdptr(valueType))
      override def category = objsCat
    }

    case class CIL_INITOBJ(valueType: TypeKind) extends Instruction {
      override def toString(): String = "CIL_INITOBJ " + valueType
      override def consumed = 1
      override def consumedTypes = ObjectReference :: Nil // actually consumes a managed pointer
      override def produced = 0
      override def category = objsCat
    }

    case class CIL_NEWOBJ(method: Symbol) extends Instruction {
      override def toString(): String = "CIL_NEWOBJ " + hostClass.fullName + method.fullName
      var hostClass: Symbol = method.owner;
      override def consumed = method.tpe.paramTypes.length
      override def consumedTypes = method.tpe.paramTypes map toTypeKind
      override def produced = 1
      override def producedTypes = List(toTypeKind(method.tpe.resultType))
      override def category = objsCat
    }

  }
}
