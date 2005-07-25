/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

import scala.tools.nsc.backend.icode.Primitives;

trait TypeStacks: Global {
  import opcodes._;

  /* This class simulates the type of the opperand
   * stack of the ICode.
   */
  class TypeStack {

    private var stack : List[Type] = Nil;
//    private val definitions = global.definitions;
    // TODO:
    //  private val typer = definitions.atyper; // !!! Using atree typer !

    private def this(stack : List[Type]) = {
      this();
      this.stack = stack;
    }

    /** Simulate the effects of an Instruction
     *  on the stack. It returns the new stack
     */
    def eval(instr: Instruction) : TypeStack = {
      log(instr.toString());
      assert(stack.length >= instr.consumed, "Invalid instruction :"+instr+" for stack "+toString());

      val result = new TypeStack(erase(production(instr)):::stack.drop(instr.consumed)); /// !!! Opt
      log("\t-> "+result.toString());
      result;

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

    /* Compute what type(s) the instruction produce on the stack */
    private def production(instr: Instruction) : List[Type] = instr match {
      case THIS(clasz) => clasz.thisType :: Nil;

      // TODO:
      //    case CONSTANT(aConstant) => typer.computeType(aConstant)::Nil;

      // TODO:
      //  case LOAD_ARRAY_ITEM() => typer.getArrayElementType(stack.tail.head)::Nil;

      case LOAD_LOCAL(local, _) => local.tpe :: Nil;

      case LOAD_FIELD(field, _) => stack.head.memberType(field) :: Nil;

      case STORE_ARRAY_ITEM() => Nil;

      case STORE_LOCAL(_,_) => Nil;

      case STORE_FIELD(_,_) => Nil;

      // TODO:
      //    case CALL_PRIMITIVE(p) => typer.computeType(p)::Nil;

      case CALL_METHOD(method, style) => style match {
        case NewInstance => method.owner.thisType :: Nil;
        case _ => method.tpe.resultType :: Nil;
      }
      case NEW(clasz) => clasz.tpe :: Nil;

      case CREATE_ARRAY(element) =>
        appliedType(definitions.ArrayClass.typeConstructor,
                    List(element)) :: Nil;

      case IS_INSTANCE(_) => definitions.BooleanClass.tpe :: Nil;

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
    private def erase(production : List[Type]) = production.filter(
      (t: Type) => !(t == definitions.UnitClass.tpe)
    );

    /* This method returns a String representation of the stack */
    override def toString() = {
      var ret : String = "";
      stack.foreach((t: Type) => ret=(t.toString()+"::")+ret);
      ret;

    }
  }

}
