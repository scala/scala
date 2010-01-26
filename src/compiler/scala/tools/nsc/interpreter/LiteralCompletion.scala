/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import util.BatchSourceFile

/** Literals, so we can pretend they are objects with methods.
 */
abstract class LiteralCompletion extends CompletionAware {
  val parent: Completion
  val global: Global

  import global._

  // TODO - figure out how to enumerate available implicit conversions.
  // def richInt = new InstanceCompletion(classOf[scala.runtime.RichInt])

  class PrimitiveCompletion(x: Type) extends CompletionAware {
    lazy val completions = x.nonPrivateMembers map (_.name.toString)
    override def follow(s: String) = {
      val member = x.nonPrivateMembers find (_.name.toString == s)
      member flatMap (m => Option(m.tpe)) map (_.resultType) map (x => new PrimitiveCompletion(x))
    }
  }

  def simpleParse(code: String): Tree = {
    val unit = new CompilationUnit(new BatchSourceFile("<console>", code))
    val scanner = new syntaxAnalyzer.UnitParser(unit)

    // only single statements
    scanner.templateStatSeq(false) match {
      case (_, List(t)) => t
      case (_, x)       => EmptyTree
    }
  }

  def completions() = Nil
  override def follow(id: String) = simpleParse(id) match {
    case Literal(c @ Constant(_))       => Some(new PrimitiveCompletion(c.tpe))
    // TODO - more AST trees.
    // case Apply(fn @ Ident(name), args)  =>
    //   classForName(name.toString) map (x => new StaticCompletion(x))
    //   None
    case x                              => None
  }
}
