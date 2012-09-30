/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package icode

/** This trait ...
 *
 *  @author  Iulian Dragos
 *  @version 1.0
 */
trait TypeStacks {
  self: ICodes =>

  import opcodes._

  /* This class simulates the type of the operand
   * stack of the ICode.
   */
  type Rep = List[TypeKind]

  object NoTypeStack extends TypeStack(Nil) { }

  class TypeStack(var types: Rep) {
    if (types.nonEmpty)
      checkerDebug("Created " + this)

    def this() = this(Nil)
    def this(that: TypeStack) = this(that.types)

    def length: Int = types.length
    def isEmpty     = length == 0
    def nonEmpty    = length != 0

    /** Push a type on the type stack. UNITs are ignored. */
    def push(t: TypeKind) = {
      if (t != UNIT)
        types = t :: types
    }

    def head: TypeKind = types.head

    /** Removes the value on top of the stack, and returns it. It assumes
     *  the stack contains at least one element.
     */
    def pop: TypeKind = {
      val t = types.head
      types = types.tail
      t
    }

    /** Return the topmost two values on the stack. It assumes the stack
     *  is large enough. Topmost element first.
     */
    def pop2: (TypeKind, TypeKind) = (pop, pop)

    /** Return the topmost three values on the stack. It assumes the stack
     *  is large enough. Topmost element first.
     */
    def pop3: (TypeKind, TypeKind, TypeKind) = (pop, pop, pop)

    /** Drop the first n elements of the stack. */
    def pop(n: Int): List[TypeKind] = {
      val prefix = types.take(n)
      types = types.drop(n)
      prefix
    }

    def apply(n: Int): TypeKind = types(n)

    /**
     * A TypeStack agrees with another one if they have the same
     * length and each type kind agrees position-wise. Two
     * types agree if one is a subtype of the other.
     */
    def agreesWith(other: TypeStack): Boolean =
      (types corresponds other.types)((t1, t2) => t1 <:< t2 || t2 <:< t1)

    /* This method returns a String representation of the stack */
    override def toString() =
      if (types.isEmpty) "[]"
      else types.mkString("[", " ", "]")

    override def hashCode() = types.hashCode()
    override def equals(other: Any): Boolean = other match {
      case x: TypeStack => x.types == types
      case _            => false
    }
  }

}
