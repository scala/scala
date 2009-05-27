/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.parsing.combinatorold.syntactical

import scala.util.parsing.ast._

//DISCLAIMER: this code is highly experimental!

/** <p>
 *    This component augments the generic parsers with support for variable binding.
 *  </p>
 *  <p>
 *    Use <code>bind</code> to decorate a parser that parses a binder (e.g.,
 *    the name of a local variable or an argument name in a list of formal
 *    arguments): besides the parser, it requires a fresh <code>Binder</code>
 *    object, which serves as a container for one or more binders with the same
 *    scope. The result of the parser is added to the binder's elements. Note
 *    that semantic equality (<code>equals</code>) is used to link a binder to
 *    its bound occurrences (along with its scope, of course).
 *  </p>
 *  <p>
 *    For example, here's how you'd write a parser (<code>p</code>) for a let
 *    construct (assuming <code>b: Binder[Name]</code>):
 *  </p><pre>
 *   "val" ~! bind(name, b) ~ ":" ~ typeP ~ "=" ~ term ~ "in" ~ in(term, b),</pre>
 *  <p>
 *    This can be read as ``The parser that matches <code>val</code> (and then
 *    does not back-track anymore), a name -- which represents a binder we'll
 *    call <code>b</code> -- a colon, a type, an equals sign, a term, the
 *    keyword <code>in</code> and finally a term where `b' is in scope.''
 *  </p>
 *  <p>
 *    The result of this parser is a nested tuple of depth 3, containing a
 *    Type, a <code>Term</code> and an <code>UnderBinder[Name, Term]</code>.
 *    Note that the binder itself is discarded (the <code>UnderBinder</code>
 *    keeps track of it).
 *  </p>
 *  <p>
 *    <code>newScope</code> makes an empty scope so that you can use
 *    <code>into</code> to pass it to a function that makes a parser
 *    whose bound variables end up in this scope:
 *    In our example, it would be used like this (with <code>b</code> free
 *    in <code>p</code>):
 *  </p><pre>
 *    newScope[Name] into { b => p }</pre>
 *  <p>
 *    Finally, <code>bound(p)</code> constructs a parser that checks that the
 *    result of <code>p</code> is bound by some binder <code>b</code> (i.e.,
 *    <code>b</code> has an element which <code>equals</code> the result of
 *    <code>p</code>) in the current scope (as delineated by
 *    <code>in(scopeP, b)</code>, where <code>p</code> is called during
 *    `scopeP'). If scoping is indeed respected, <code>bound(p)</code>
 *    wraps the result of <code>p</code> in a <code>BoundElement</code>.
 *  </p>
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
  def newScope[T <: NameElement] = success(new Scope[T])

  def nested[T <: NameElement](s: Scope[T]) = success(s.nested)

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
    def apply(in: Input): ParseResult[Unit] = {
      binderParser(in).map(x => scope.addBinder(x))
    }
  }

  /** <p>
   *    Parse something that is in the scope of the given binders.
   *  </p>
   *  <p>
   *    During the execution of <code>scopeParser</code>, the binders in
   *    <code>binder</code> are active: see <code>bound</code> for more
   *    information. The result of the decorated parser is wrapped in an
   *    <code>UnderBinder</code>.
   *  </p>
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
    boundElementParser ^? ({case x: NameElement if !findScope(x).isEmpty => BoundElement(x, findScope(x).get)}, (x: bt) => """Unbound variable `"""+x+"""'""")


  private var binderEnv: BinderEnv = EmptyBinderEnv
  protected def inScope[bt <: NameElement, res](scope: Scope[bt])(block: => res) :res = {
    val oldEnv = binderEnv // save old environment

    // bring binders in the scope in scope
    for(b <- scope) binderEnv = binderEnv.extend(b, scope)

    // return the result of running block (in which these binders are in scope)
    // before returning, the binderEnv is restored to its old value
    return_{scope.onEnter; block} andDo {scope.onLeft; binderEnv = oldEnv}
  }

  protected def findScope[bt <: NameElement](x: bt): Option[Scope[bt]] = binderEnv(x)
}
