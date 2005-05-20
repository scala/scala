/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */


// $Id$


import scalac.symtab._;

import scalac.atree._;

package scala.tools.scalac.icode {




/** This class represents an instruction of intermediate code */
abstract class ICInstruction {

  /** This abstract method returns the number of used elements on the stack */
  def consumed : int = 0; // !!!

  /** This abstract method returns the number of produced elements on the stack */
  def produced : int = 0; // !!!

  /** This method returns the difference of size of the stack when the instruction is used */
  def difference = produced-consumed;
}

//##################################################
// Instruction cases

/** This class represents a THIS instruction
  * Stack: ...
  *    ->: ...:ref
  */
case class THIS(clasz: Symbol) extends ICInstruction {
  /** Returns a string representation of this constant */
  override def toString(): String = "THIS";

  override def consumed = 0;
  override def produced = 1;
}

/** This class retpresents a CONSTANT instruction
  * Stack: ...
  *    ->: ...:constant
  */
case class CONSTANT(constant: AConstant) extends ICInstruction{
  /** Returns a string representation of this constant */
  override def toString(): String = "CONSTANT ("+constant.toString()+")";

  override def consumed = 0;
  override def produced = 1;
}

/** This class represents a LOAD_ARRAY_ITEM instruction
  * Stack: ...:array[a](Ref):index(Int)
  *    ->: ...:element(a)
  */
case class LOAD_ARRAY_ITEM() extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String = "LOAD_ARRAY_ITEM";

  override def consumed = 2;
  override def produced = 1;
}

/** This class represents a LOAD_LOCAL instruction
  * Stack: ...
  *    ->: ...:value
  */
case class LOAD_LOCAL(local: Symbol, isArgument: boolean) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String = "LOAD_LOCAL "+local.toString(); //+isArgument?" (argument)":"";

  override def consumed = 0;
  override def produced = 1;
}

/** This class represents a LOAD_FIELD instruction
  * Stack: ...:ref
  *    ->: ...:value
  */
case class LOAD_FIELD(field: Symbol, isStatic: boolean) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String = "LOAD_FIELD "+field.toString(); //+isStatic?" (static)":"";

  override def consumed = 1;
  override def produced = 1;
}

/** This class represents a STORE_ARRAY_ITEM instruction
  * Stack: ...:array[a](Ref):index(Int):value(a)
  *    ->: ...
  */
case class STORE_ARRAY_ITEM() extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String = "STORE_ARRAY_ITEM";

  override def consumed = 3;
  override def produced = 0;
}

/** This class represents a STORE_LOCAL instruction
  * Stack: ...:value
  *    ->: ...
  */
case class STORE_LOCAL(local: Symbol, isArgument: boolean) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String = "STORE_LOCAL "+local.toString(); //+isArgument?" (argument)":"";

  override def consumed = 1;
  override def produced = 0;
}

/** This class represents a STORE_FIELD instruction
  * Stack: ...:ref:value
  *    ->: ...
  */
case class STORE_FIELD(field: Symbol, isStatic: boolean) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String = "STORE_FIELD "+field.toString(); //+isStatic?" (static)":"";

  override def consumed = 2;
  override def produced = 0;
}

/** This class represents a CALL_PRIMITIVE instruction
  * Stack: ...:arg1:arg2:...:argn
  *    ->: ...:result
  */
case class CALL_PRIMITIVE(primitive: APrimitive) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String ="CALL "+primitive.toString();

  override def consumed = primitive match {
    case (APrimitive$Negation(_)) => 1;
    case (APrimitive$Test(_,_,true)) => 1;
    case (APrimitive$Test(_,_,false)) => 2;
    case (APrimitive$Comparison(_,_)) => 2;
    case (APrimitive$Arithmetic(_,_)) => 2;
    case (APrimitive$Logical(_,_)) => 2;
    case (APrimitive$Shift(_,_)) => 2;
    case (APrimitive$Conversion(_,_)) => 1;
    case (APrimitive$ArrayLength(_)) => 1;
    case (APrimitive$StringConcat(_,_)) => 2;
  }
  override def produced = 1;
}

/** This class represents a CALL_METHOD instruction
  * STYLE: dynamic / static(StaticInstance)
  * Stack: ...:ref:arg1:arg2:...:argn
  *    ->: ...:result
  *
  * STYLE: new - unused by jvm
  * Stack: ...:arg1:arg2:...:argn
  *    ->: ...:ref
  *
  * STYLE: static(StaticClass)
  * Stack: ...:arg1:arg2:...:argn
  *    ->: ...:result
  *
  */
case class CALL_METHOD(method: Symbol, style: AInvokeStyle) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String ="CALL_METHOD "+method.toString()+" ("+style.toString()+")";

  override def consumed = {
    var result = method.getType().valueParams().length;
    result = result + (style match
  {case(AInvokeStyle.Dynamic) => 1
   case(AInvokeStyle.StaticInstance) => 1
   case(_) => 0 });
    result;
  }
  override def produced = 1;
}

/** This class represents a NEW instruction
  * Stack: ...:
  *    ->: ...:ref
  */
  case class NEW(clasz: Symbol) extends ICInstruction {
    /** Returns a string representation of this instruction */
    override def toString(): String = "NEW "+clasz.toString();

    override def consumed = 0;
    override def produced = 1;
  }


  /** This class represents a CREATE_ARRAY instruction
  * Stack: ...:size(int)
  *    ->: ...:arrayref
  */
case class CREATE_ARRAY(element: Type) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String ="CREATE_ARRAY "+element.toString();

  override def consumed = 1;
  override def produced = 1;
}

  /** This class represents a IS_INSTANCE instruction
  * Stack: ...:ref
  *    ->: ...:result(boolean)
  */
case class IS_INSTANCE(typ: Type) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String ="IS_INSTANCE "+typ.toString();

  override def consumed = 1;
  override def produced = 1;
}

/** This class represents a CHECK_CAST instruction
  * Stack: ...:ref(oldtype)
  *    ->: ...:ref(typ <=: oldtype)
  */
case class CHECK_CAST(typ: Type) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String ="CHECK_CAST "+typ.toString();

  override def consumed = 1;
  override def produced = 1;
}

/** This class represents a SWITCH instruction
  * Stack: ...:index(int)
  *    ->: ...:
  */
case class SWITCH(tags: Array[Array[int]], labels: List[IBasicBlock]) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String ="SWITCH ...";

  override def consumed = 1;
  override def produced = 0;
}

/** This class represents a JUMP instruction
  * Stack: ...
  *    ->: ...
  */
case class JUMP(where: IBasicBlock) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String ="JUMP "+where.label;

  override def consumed = 0;
  override def produced = 0;
}

/** This class represents a CJUMP instruction
  * It compares the two values on the stack with the 'cond' test operator
  * Stack: ...:value1:value2
  *    ->: ...
  */
case class CJUMP(successBlock: IBasicBlock, failureBlock: IBasicBlock, cond: ATestOp) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String ="CJUMP "+cond.toString()+" ? "+successBlock.label+" : "+failureBlock.label;

  override def consumed = 2;
  override def produced = 0;
}

/** This class represents a CZJUMP instruction
  * It compares the one value on the stack and zero with the 'cond' test operator
  * Stack: ...:value:
  *    ->: ...
  */
case class CZJUMP(successBlock: IBasicBlock, failureBlock: IBasicBlock, cond: ATestOp) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String ="CZJUMP "+cond.toString()+" ? "+successBlock.label+" : "+failureBlock.label;

  override def consumed = 1;
  override def produced = 0;
}


/** This class represents a RETURN instruction
  * Stack: ...
  *    ->: ...
  */
case class RETURN() extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String ="RETURN";

  override def consumed = 0;
  override def produced = 0;
}

/** This class represents a THROW instruction
  * Stack: ...:Throwable(Ref)
  *    ->: ...:
  */
case class THROW() extends ICInstruction {
/** Returns a string representation of this instruction */
  override def toString(): String ="THROW";

  override def consumed = 1;
  override def produced = 0;
}

/** This class represents a DROP instruction
  * Stack: ...:something
  *    ->: ...
  */
case class DROP (typ: Type) extends ICInstruction {
  /** Returns a string representation of this instruction */
  override def toString(): String ="DROP "+typ.toString();

  override def consumed = 1;
  override def produced = 0;
}

  /** This class represents a DUP instruction
  * Stack: ...:something
  *    ->: ...:something:something
  */
  case class DUP (typ: Type) extends ICInstruction {
    /** Returns a string representation of this instruction */
    override def toString(): String ="DUP";

    override def consumed = 1;
    override def produced = 2;
  }

  /** This class represents a MONITOR_ENTER instruction
  * Stack: ...:object(ref)
  *    ->: ...:
  */
  case class MONITOR_ENTER() extends ICInstruction {

    /** Returns a string representation of this instruction */
    override def toString(): String ="MONITOR_ENTER";

    override def consumed = 1;
    override def produced = 0;
  }

  /** This class represents a MONITOR_EXIT instruction
  * Stack: ...:object(ref)
  *    ->: ...:
  */
  case class MONITOR_EXIT() extends ICInstruction {

    /** Returns a string representation of this instruction */
    override def toString(): String ="MONITOR_EXIT";

    override def consumed = 1;
    override def produced = 0;
  }

}
