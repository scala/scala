/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.parsing.combinator.syntactical

import scala.util.parsing.ast._

// DISCLAIMER: this code is not well-tested -- consider it beta-quality!

/** This component augments the generic parsers with support for variable binding.
 *
 * Use `bind' to decorate a parser that parses a binder (e.g., the name of a local variable or
 * an argument name in a list of formal arguments): besides the parser, it requires a fresh
 * `Binder' object, which serves as a container for one or more binders with the same scope.
 * The result of the parser is added to the binder's elements. Note that semantic equality (`equals')
 * is used to link a binder to its bound occurrences (along with its scope, of course).
 *
 * For example, here's how you'd write a parser (`p') for a let construct (assuming b: Binder[Name]):
 *   "val" ~! bind(name, b) ~ ":" ~ typeP ~ "=" ~ term ~ "in" ~ in(term, b),
 *
 * This can be read as ``The parser that matches `val' (and then does not back-track anymore),
 * a name -- which represents a binder we'll call `b' -- a colon, a type, an equals sign, a term,
 * the keyword `in' and finally a term where `b' is in scope.''
 *
 * The result of this parser is a nested tuple of depth 3, containing a Type, a Term and
 * an UnderBinder[Name, Term]. Note that the binder itself is discarded (the UnderBinder keeps track of it).
 *
 * `newScope' makes an empty scope so that you can use `into' to pass it to a function that makes a parser
 * whose bound variables end up in this scope:
 *  In our example, it would be used like this (with `b' free in `p'): <pre>newScope[Name] into { b => p }</pre>
 *
 * Finally, `bound(p)' constructs a parser that checks that the result of `p' is bound by some binder `b'
 * (i.e., `b' has an element which `equals' the result of `p') in the current scope (as delineated by
 * `in(scopeP, b)', where `p' is called during `scopeP'). If scoping is indeed respected, `bound(p)'
 * wraps the result of `p' in a `BoundElement'.
 *
 * @author Adriaan Moors
 */
trait BindingParsers extends Parsers with Binders {
  /** A shortcut for `success(new Scope[t])'
   *
   * Typically used in combination with the `into' combiner as follows:
   * <pre>newScope[Name] into { b =>
   *    "val" ~! bind(name, b) ~ ":" ~ typeP ~ "=" ~ term ~ "in" ~ in(term, b)}</pre>
   */
  def newScope[t <: NameElement] = success(new Scope[t])

  def nested[t <: NameElement](s: Scope[t]) = success(s.nested)

  // TODO: make `bind' and `in' methods of Scope?

  /** Generate a UnitParser that parses a binder
   *
   * The result of `binderParser' (a binder) will be added to the binder container `binder',
   * so that `b' can later be used to refer to the binder parsed by `binderParser' (e.g., in the
   *  `in' combinator)
   *
   * @param binderParser a parser that parses a binder (e.g., a variable name)
   * @param scope        a scope that will contain the parsed binder
   * @return a parser with the same behaviour as `binderParser', except that its result will be
   *          added to `scope' and not returned.
   */
  def bind[bt <: NameElement](binderParser: Parser[bt], scope: Scope[bt]) = new UnitParser {
    def apply(in: Input): ParseResult[unit] = {
      binderParser(in).map(x => scope.addBinder(x))
    }
  }

  /** Parse something that is in the scope of the given binders.
   *
   * During the execution of `scopeParser', the binders in `binder' are active:
   * see `bound' for more information. The result of the decorated parser is wrapped in an `UnderBinder'
   *
   * @param scopeParser the parser that parses something that is in the scope of `binder'
   * @param binder      a container of binders, typically populated by `bind'
   * @return a parser that has the same behaviour as `scopeParser', but whose result is wrapped
   *          in an `UnderBinder'
   */
  def in[scopeT <% Mappable[scopeT], bt <: NameElement ](scopeParser: Parser[scopeT], scope: Scope[bt]) = new Parser[UnderBinder[bt, scopeT]] {
    def apply(in: Input): ParseResult[UnderBinder[bt, scopeT]] = inScope(scope){
      scopeParser(in).map(x => UnderBinder(scope, x))
    }
  }

  /** A parser that checks that there are no unbound variables.
   *
   * `bound(p)' succeeds if the element parsed by p is bound by an active binder (see `in')
   *
   * @param boundElementParser a parser that parses an element that must be bound
   * @return a parser that succeeds if the element parsed by `boundElementParser' was bound,
   *          wrapping its result in a `BoundElement'
   */
  def bound[bt <: NameElement](boundElementParser: Parser[bt]) =
    boundElementParser ^? ({case x: bt if !findScope(x).isEmpty => BoundElement(x, findScope(x).get)}, (x: bt) => """Unbound variable `"""+x+"""'""")


  private var binderEnv: BinderEnv = EmptyBinderEnv
  protected def inScope[bt <: NameElement, res](scope: Scope[bt])(block: => res) :res = {
    val oldEnv = binderEnv // save old environment

    // bring binders in the scope in scope
    for(val b <- scope) binderEnv = binderEnv.extend(b, scope)

    // return the result of running block (in which these binders are in scope)
    // before returning, the binderEnv is restored to its old value
    return_{scope.onEnter; block} andDo {scope.onLeft; binderEnv = oldEnv}
  }

  protected def findScope[bt <: NameElement](x: bt): Option[Scope[bt]] = binderEnv(x)
}
