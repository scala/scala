/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package icode

trait Primitives { self: ICodes =>

  /** This class represents a test operation. */
  sealed abstract class TestOp {

    /** Returns the negation of this operation. */
    def negate(): TestOp

    /** Returns a string representation of this operation. */
    override def toString(): String

    /** used only from GenASM */
    def opcodeIF(): Int

    /** used only from GenASM */
    def opcodeIFICMP(): Int
  }

  /** An equality test */
  case object EQ extends TestOp {
    def negate() = NE
    override def toString() = "EQ"
    override def opcodeIF()     = scala.tools.asm.Opcodes.IFEQ
    override def opcodeIFICMP() = scala.tools.asm.Opcodes.IF_ICMPEQ
  }

  /** A non-equality test */
  case object NE extends TestOp {
    def negate() = EQ
    override def toString() = "NE"
    override def opcodeIF()     = scala.tools.asm.Opcodes.IFNE
    override def opcodeIFICMP() = scala.tools.asm.Opcodes.IF_ICMPNE
  }

  /** A less-than test */
  case object LT extends TestOp {
    def negate() = GE
    override def toString() = "LT"
    override def opcodeIF()     = scala.tools.asm.Opcodes.IFLT
    override def opcodeIFICMP() = scala.tools.asm.Opcodes.IF_ICMPLT
  }

  /** A greater-than-or-equal test */
  case object GE extends TestOp {
    def negate() = LT
    override def toString() = "GE"
    override def opcodeIF()     = scala.tools.asm.Opcodes.IFGE
    override def opcodeIFICMP() = scala.tools.asm.Opcodes.IF_ICMPGE
  }

  /** A less-than-or-equal test */
  case object LE extends TestOp {
    def negate() = GT
    override def toString() = "LE"
    override def opcodeIF()     = scala.tools.asm.Opcodes.IFLE
    override def opcodeIFICMP() = scala.tools.asm.Opcodes.IF_ICMPLE
  }

  /** A greater-than test */
  case object GT extends TestOp {
    def negate() = LE
    override def toString() = "GT"
    override def opcodeIF()     = scala.tools.asm.Opcodes.IFGT
    override def opcodeIFICMP() = scala.tools.asm.Opcodes.IF_ICMPGT
  }

  /** This class represents an arithmetic operation. */
  class ArithmeticOp {

    /** Returns a string representation of this operation. */
    override def toString(): String = this match {
      case ADD => "ADD"
      case SUB => "SUB"
      case MUL => "MUL"
      case DIV => "DIV"
      case REM => "REM"
      case NOT => "NOT"
      case _   => throw new RuntimeException("ArithmeticOp unknown case")
    }
  }

  /** An arithmetic addition operation */
  case object ADD extends ArithmeticOp

  /** An arithmetic subtraction operation */
  case object SUB extends ArithmeticOp

  /** An arithmetic multiplication operation */
  case object MUL extends ArithmeticOp

  /** An arithmetic division operation */
  case object DIV extends ArithmeticOp

  /** An arithmetic remainder operation */
  case object REM extends ArithmeticOp

  /** Bitwise negation. */
  case object NOT extends ArithmeticOp
}

