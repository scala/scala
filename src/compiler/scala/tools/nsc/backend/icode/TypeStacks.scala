/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

import scala.tools.nsc.backend.icode.Primitives;

trait TypeStacks requires ICodes {
  import opcodes._;
  import global.{Symbol, Type, definitions};

  /* This class simulates the type of the opperand
   * stack of the ICode.
   */
  type Rep = List[TypeKind];

  class TypeStack {
    var types: Rep = Nil;

    def this(stack: Rep) = {
      this();
      this.types = stack;
    }

    def this(that: TypeStack) = {
      this(that.types);
    }

    def length: Int = types.length;

    /** Push a type on the type stack. UNITs are ignored. */
    def push(t: TypeKind) =
      if (t != UNIT)
        types = t :: types;

    /** Removes the value on top of the stack, and returns it. It assumes
     *  the stack contains at least one element.
     */
    def pop: TypeKind = {
      val t = types.head;
      types = types.tail;
      t
    }

    /** Return the topmost two values on the stack. It assumes the stack
     *  is large enough. Topmost element first.
     */
    def pop2: Pair[TypeKind, TypeKind] = Pair(pop, pop);

    /** Return the topmost three values on the stack. It assumes the stack
     *  is large enough. Topmost element first.
     */
    def pop3: Triple[TypeKind, TypeKind, TypeKind] = Triple(pop, pop, pop);

    /**
     * A TypeStack aggress with another one if they have the same
     * length and each type kind agrees position-wise. Two
     * types agree if they are subtypes of one another.
     */
    def agreesWith(other: TypeStack): Boolean = (
      (types.length == other.types.length) &&
      List.forall2(types, other.types) ((t1, t2) => t1 <:< t2 || t2 <:< t1)
    );

    def mergeWith(that: TypeStack): TypeStack = {
      def merge(a: TypeStack, b: TypeStack): TypeStack = {
        val lst = List.map2(a.types, b.types) ((k1, k2) => k1 match {
          case REFERENCE(cls1) =>
            val REFERENCE(cls2) = k2;
            lub(k1,k2);
          case _ => k1;
        });
        new TypeStack(lst)
      }

      assert(this agreesWith that,
             "Incompatible type stacks: " + this + ", " + that);
      merge(this, that)
    }

    /* This method returns a String representation of the stack */
    override def toString() = {
      (types.foldLeft(new StringBuffer("")) ((buf, k) => buf.append(" ").append(k))).toString();
    }

    override def equals(other: Any): Boolean = (
      other.isInstanceOf[TypeStack] &&
      List.forall2(other.asInstanceOf[TypeStack].types, types)((a, b) => a == b)
    );
  }

}
