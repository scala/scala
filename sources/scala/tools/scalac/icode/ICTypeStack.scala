/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.atree._;
import scalac.symtab._;
import scalac.{Global => scalac_Global};

package scala.tools.scalac.icode {



/* This class simulates the type of the opperand
 * stack of the ICode. */
class ICTypeStack() {

  // ##################################################
  // Private fields
  private var stack : List[Type] = Nil;

  private val global = scalac_Global.instance;

  private val definitions = global.definitions;

  private val typer = definitions.atyper; // !!! Using atree typer !

  // ##################################################
  // Private constructor

  private def this(stack : List[Type]) = {
    this();
    this.stack = stack;
  }

  // ##################################################
  // Public method

  /* This methods simulate the effect of an ICInstruction
   * on the stack. It returns the new stack */
  def eval(instr: ICInstruction) : ICTypeStack = {
    global.log(instr.toString());
    if (stack.length < instr.consumed) {
      global.log("Invalid instruction :"+instr+" for stack "+toString());
      this;
    } else {
      val result = new ICTypeStack(erase(production(instr)):::stack.drop(instr.consumed)); /// !!! Opt
      global.log("\t-> "+result.toString());
      result;
    }
  }

  // ##################################################
  // Public methods - Operation on the stack

  /* Push a new type */
  def push(t: Type) = stack = t::stack;

  /* Returns the topmost type of the stack */
  def head = stack.head;

  /* Returns the stack without the topmost element */
  def tail = stack.tail;

  /* Is the stack empty ? */
  def isEmpty = stack.isEmpty;

  // ##################################################
  // Private methods

  /* Compute what type(s) the instruction produce on the stack */
  private def production(instr: ICInstruction) : List[Type] = instr match {
    case THIS(clasz) => clasz.thisType()::Nil;

    case CONSTANT(aConstant) => typer.computeType(aConstant)::Nil;

    case LOAD_ARRAY_ITEM() => typer.getArrayElementType(stack.tail.head)::Nil;

    case LOAD_LOCAL(local, _) => local.getType()::Nil;

    case LOAD_FIELD(field, _) => stack.head.memberStabilizedType(field)::Nil; // ??

    case STORE_ARRAY_ITEM() => Nil;

    case STORE_LOCAL(_,_) => Nil;

    case STORE_FIELD(_,_) => Nil;

    case CALL_PRIMITIVE(p) => typer.computeType(p)::Nil;

    case CALL_METHOD(method, style) => style match {
      //case AInvokeStyle.Dynamic => stack.head.memberStabilizedType(method)::Nil; // ???
      //case AInvokeStyle.Dynamic => stack.drop(method.getType().valueParams().length).head.resultType()::Nil;
      case AInvokeStyle.New => method.owner().thisType()::Nil;

      case _ => method.resultType()::Nil; // Valable pour Dynamic
      //case _ => method.owner().thisType().memberStabilizedType(method)::Nil; // ???
    }
    case NEW(clasz) => clasz.getType()::Nil;

    case CREATE_ARRAY(element) => definitions.array_TYPE(element)::Nil;

    case IS_INSTANCE(_) => definitions.boolean_TYPE()::Nil;

    case CHECK_CAST(typ) => typ::Nil;

    case SWITCH(_,_) => Nil;

    case JUMP(_) => Nil;

    case CJUMP(_,_,_) => Nil;

    case CZJUMP(_,_,_) => Nil;

    case RETURN() => Nil;

    case THROW() => Nil;

    case DROP(_) => Nil;

    case DUP(_) => stack.head::stack.head::Nil;

    case MONITOR_ENTER() => Nil;

    case MONITOR_EXIT() => Nil;
  }

  /* This method kill all the produced *Unit* elements */
  // !!! Hack
  private def erase(production : List[Type]) = production.filter((t: Type) => t match {
    case Type$UnboxedType(TypeTags.UNIT) => false;
    case _ => true
  });

  /* This method returns a String representation of the stack */
  // !!! POUAH SALE !
  override def toString() = {
    var ret : String = "";
    stack.foreach((t: Type) => ret=(t.toString()+"::")+ret);
    ret;

  }
}
}
