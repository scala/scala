/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import CodeHandlers.NoSuccess
import scala.util.control.ControlThrowable

/**
 *  The start of a simpler interface for utilizing the compiler with piecemeal
 *  code strings.  The "T" here could potentially be a Tree, a Type, a Symbol,
 *  a Boolean, or something even more exotic.
 */
trait CodeHandlers[T] {
  self =>

  // Expressions are composed of operators and operands.
  def expr(code: String): T

  // A declaration introduces names and assigns them types.
  // It can form part of a class definition (§5.1) or of a refinement in a compound type (§3.2.7).
  // (Ed: aka abstract members.)
  //
  // ‘val’ ValDcl | ‘var’ VarDcl | ‘def’ FunDcl | ‘type’ {nl} TypeDcl
  def decl(code: String): T

  // A definition introduces names that denote terms or types.
  // It can form part of an object or class definition or it can be local to a block.
  // (Ed: aka concrete members.)
  //
  // ‘val’ PatDef | ‘var’ VarDef | ‘def’ FunDef | ‘type’ {nl} TypeDef |
  // [‘case’] ‘class’ ClassDef | [‘case’] ‘object’ ObjectDef | ‘trait’ TraitDef
  def defn(code: String): T

  // An import clause has the form import p.I where p is a stable identifier (§3.1) and I is an import expression.
  def impt(code: String): T

  // Statements occur as parts of blocks and templates.
  // A statement can be an import, a definition or an expression, or it can be empty.
  // Statements used in the template of a class definition can also be declarations.
  def stmt(code: String): T
  def stmts(code: String): Seq[T]

  object opt extends CodeHandlers[Option[T]] {
    val handler: PartialFunction[Throwable, Option[T]] = {
      case _: NoSuccess => None
    }
    val handlerSeq: PartialFunction[Throwable, Seq[Option[T]]] = {
      case _: NoSuccess => Nil
    }

    def expr(code: String)   = try Some(self.expr(code)) catch handler
    def decl(code: String)   = try Some(self.decl(code)) catch handler
    def defn(code: String)   = try Some(self.defn(code)) catch handler
    def impt(code: String)   = try Some(self.impt(code)) catch handler
    def stmt(code: String)   = try Some(self.stmt(code)) catch handler
    def stmts(code: String)  = try (self.stmts(code) map (x => Some(x))) catch handlerSeq
  }
}

object CodeHandlers {
  def incomplete() = throw CodeIncomplete
  def fail(msg: String) = throw new CodeException(msg)

  trait NoSuccess extends ControlThrowable
  class CodeException(msg: String) extends RuntimeException(msg) with NoSuccess { }
  object CodeIncomplete extends CodeException("CodeIncomplete")
}
