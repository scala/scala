/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.parsing.ast

import scala.collection.AbstractIterable
import scala.collection.mutable
import language.implicitConversions

//DISCLAIMER: this code is highly experimental!

  // TODO: avoid clashes when substituting
  // TODO: check binders in the same scope are distinct

/** This trait provides the core ''Scrap-Your-Boilerplate'' abstractions as
 *  well as implementations for common datatypes.
 *
 *  (Based on Ralf Lämmel's [[http://homepages.cwi.nl/~ralf/syb3/ SYB papers]].)
 *
 *  @author Adriaan Moors
 */
@deprecated("This class will be removed", "2.10.0")
trait Mappable {
  trait Mapper { def apply[T <% Mappable[T]](x: T): T } /* TODO: having type `Forall T. T => T` is too strict:
  sometimes we want to allow `Forall T >: precision. T => T` for some type `precision`, so that,
  beneath a certain threshold, we have some leeway.
  concretely: to use gmap for substitution, we simply require that ast nodes are mapped to ast nodes,
  we can't require that the type is preserved precisely: a Name may map to e.g., a MethodCall
  */

  trait Mappable[T] {
    // one-layer traversal
    def gmap(f: Mapper): T
    //  everywhere f x = f (gmapT (everywhere f) x)
    def everywhere(f: Mapper)(implicit c: T => Mappable[T]): T =
      f(gmap(new Mapper { def apply[T <% Mappable[T]](x: T): T = x.everywhere(f)}))
  }

  implicit def StringIsMappable(s: String): Mappable[String] =
    new Mappable[String] {
      def gmap(f: Mapper): String = f(s)
    }

  implicit def ListIsMappable[t <% Mappable[t]](xs: List[t]): Mappable[List[t]] =
    new Mappable[List[t]] {
      def gmap(f: Mapper): List[t] = (for (x <- xs) yield f(x)).toList
    }

  implicit def OptionIsMappable[t <% Mappable[t]](xs: Option[t]): Mappable[Option[t]] =
    new Mappable[Option[t]] {
      def gmap(f: Mapper): Option[t] = (for (x <- xs) yield f(x))
    }
}

/** This component provides functionality for enforcing variable binding
 *  during parse-time.
 *
 *  When parsing simple languages, like Featherweight Scala, these parser
 *  combinators will fully enforce the binding discipline. When names are
 *  allowed to be left unqualified, these mechanisms would have to be
 *  complemented by an extra phase that resolves names that couldn't be
 *  resolved using the naive binding rules. (Maybe some machinery to
 *  model `implicit` binders (e.g., `this` and imported qualifiers)
 *  and selection on a binder will suffice?)
 *
 * @author Adriaan Moors
 */
trait Binders extends AbstractSyntax with Mappable {
  /** A `Scope` keeps track of one or more syntactic elements that represent bound names.
   *  The elements it contains share the same scope and must all be distinct, as determined by `==`.
   *
   *  A `NameElement` `n` in the AST that is conceptually bound by a `Scope` `s`, is replaced by a
   *  `BoundElement(n, s)`. (For example, in `val x:Int=x+1`, the first `x` is modelled by a
   *  Scope `s` that contains `x` and the second `x` is represented by a `BoundElement(x, s)`)
   *  The term (`x+1`) in scope of the Scope becomes an `UnderBinder(s, x+1)`.
   *
   *  A `NameElement` `n` is bound by a `Scope` `s` if it is wrapped as a `BoundElement(n, s)`, and
   *  `s` has a binder element that is semantically equal (`equals` or `==`) to `n`.
   *
   *  A `Scope` is represented textually by its list of binder elements, followed by the scope's `id`.
   *  For example: `[x, y]!1` represents the scope with `id` `1` and binder elements `x` and `y`.
   *  (`id` is solely used for this textual representation.)
   */
  class Scope[binderType <: NameElement] extends AbstractIterable[binderType] with Iterable[binderType] {
    private val substitution: mutable.Map[binderType, Element] =
      new mutable.LinkedHashMap[binderType, Element] // a LinkedHashMap is ordered by insertion order -- important!

    /** Returns a unique number identifying this Scope (only used for representation purposes). */
    val id: Int = _Binder.genId

    /** Returns the binders in this scope.
     *  For a typical let-binding, this is just the variable name. For an argument list to a method body,
     *  there is one binder per formal argument.
     */
    def iterator = substitution.keysIterator

    /** Return the `i`th binder in this scope. */
    def apply(i: Int): binderType = this.iterator.toList(i)

    /** Returns true if this container has a binder equal (as determined by `==`) to `b`. */
    def binds(b: binderType): Boolean = substitution.contains(b)

    def indexFor(b: binderType): Option[Int] = {
      val iter = this.iterator.zipWithIndex
      for ((that, count) <- iter) {
        if (that.name == b.name) // TODO: why do name equals and structural equals differ?
          return Some(count + 1)
        else
          Console.println(that+"!="+b)
      }

      None
    }

    /** Adds a new binder, for example the variable name in a local variable declaration.
     *
     * @param b a new binder that is distinct from the existing binders in this scope,
     *           and shares their conceptual scope. `canAddBinder(b)` must hold.
     * @return `binds(b)` and `getElementFor(b) eq b` will hold.
     */
    def addBinder(b: binderType) { substitution += Pair(b, b) }

    // TODO: strengthen this condition so that no binders may be added after this scope has been
    //       linked to its `UnderBinder` (i.e., while parsing, BoundElements may be added to the Scope
    //       associated to the UnderBinder, but after that, no changes are allowed, except for substitution)?
    /** `canAddElement` indicates whether `b` may be added to this scope.
     *
     *
     * @return true if `b` had not been added yet
     */
    def canAddBinder(b: binderType): Boolean = !binds(b)

    /** ''Replaces'' the bound occurrences of a contained binder by their new value.
     *  The bound occurrences of `b` are not actually replaced; the scope keeps track
     *  of a substitution that maps every binder to its current value. Since a `BoundElement` is
     *  a proxy for the element it is bound to by its binder, `substitute` may thus be thought of
     *  as replacing all the bound occurrences of the given binder `b` by their new value `value`.
     *
     *  @param b    the binder whose bound occurrences should be given a new value. `binds(b)` must hold.
     *  @param value the new value for the bound occurrences of `b`
     *  @return `getElementFor(b) eq value` will hold.
     */
    def substitute(b: binderType, value: Element): Unit = substitution(b) = value

    /** Returns the current value for the bound occurrences of `b`.
     *
     *  @param b the contained binder whose current value should be returned `binds(b)` must hold.
     */
    def getElementFor(b: binderType): Element = substitution(b)

    override def toString: String =  this.iterator.toList.mkString("[",", ","]")+"!"+id // TODO show substitution?

    /** Returns a list of strings that represent the binder elements, each tagged with this scope's id. */
    def bindersToString: List[String] = (for(b <- this.iterator) yield b+"!"+id).toList

    /** Return a new inheriting scope that won't check whether binding is respected until the scope is left (so as to support forward references). */
    def allowForwardRef: Scope[binderType] = this // TODO

    /** Return a nested scope -- binders entered into it won't be visible in this scope, but if this scope allows forward references,
     *  the binding in the returned scope also does, and thus the check that all variables are bound is deferred until this scope is left.
     */
    def nested: Scope[binderType] = this // TODO

    def onEnter() {}
    def onLeft() {}
  }


  trait BindingSensitive {
    // would like to specify this as one method:
    // def alpha_==[t <: NameElement](other: BoundElement[t]): Boolean
    // def alpha_==[bt <: binderType, st <: elementT](other: UnderBinder[bt, st]): Boolean
  }

  /** A `BoundElement` is bound in a certain scope `scope`, which keeps track of the actual element that
   *  `el` stands for.
   *
   *  A `BoundElement` is represented textually by its bound element, followed by its scope's `id`.
   *  For example: `x@1` represents the variable `x` that is bound in the scope with `id` `1`.
   *
   *  @note `scope.binds(el)` holds before and after.
   */
  case class BoundElement[boundElement <: NameElement](el: boundElement, scope: Scope[boundElement]) extends NameElement with Proxy with BindingSensitive {
    /** Returns the element this `BoundElement` stands for.
     *  The `Proxy` trait ensures `equals`, `hashCode` and `toString` are forwarded to
     *  the result of this method.
     */
    def self: Element = scope.getElementFor(el)

    def name = self.asInstanceOf[NameElement].name // TODO: this is only safe when substituted to a NameElement, which certainly isn't required -- I want dynamic inheritance! :)

    // decorate element's representation with the id of the scope it's bound in
    override def toString: String =  super.toString+"@"+scope.id

    def alpha_==[t <: NameElement](other: BoundElement[t]): Boolean = scope.indexFor(el) == other.scope.indexFor(other.el)
  }

  /** A variable that escaped its scope (i.e., a free variable) -- we don't deal very well with these yet. */
  class UnboundElement[N <: NameElement](private val el: N) extends NameElement {
    def name = el.name+"@??"
  }

  // this is useless, as Element is a supertype of BoundElement --> the coercion will never be inferred
  // if we knew a more specific type for the element that the bound element represents, this could make sense
  // implicit def BoundElementProxy[t <: NameElement](e: BoundElement[t]): Element = e.self

  /** Represents an element with variables that are bound in a certain scope. */
  class UnderBinder[binderType  <: NameElement, elementT <% Mappable[elementT]](val scope: Scope[binderType], private[Binders] val element: elementT) extends Element with BindingSensitive {
    override def toString: String = "(" + scope.toString + ") in { "+element.toString+" }"

    /** Alpha-equivalence -- TODO
     *  Returns true if the `element` of the `other` `UnderBinder` is equal to this `element` up to alpha-conversion.
     *
     *  That is, regular equality is used for all elements but `BoundElement`s: such an element is
     *  equal to a `BoundElement` in `other` if their binders are equal. Binders are equal if they
     *  are at the same index in their respective scope.
     *
     *  Example:
     *  {{{
     *    UnderBinder([x, y]!1, x@1) alpha_== UnderBinder([a, b]!2, a@2)
     *    ! (UnderBinder([x, y]!1, y@1) alpha_== UnderBinder([a, b]!2, a@2))
     *  }}}
     */
    /*def alpha_==[bt <: binderType, st <: elementT](other: UnderBinder[bt, st]): Boolean = {
       var result = true

       // TODO: generic zip or gmap2
       element.gmap2(other.element, new Mapper2 {
         def apply[s  <% Mappable[s], t  <% Mappable[t]](x :{s, t}): {s, t} = x match {
           case {be1: BoundElement[_], be2: BoundElement[_]} => result == result && be1.alpha_==(be2) // monadic gmap (cheating using state directly)
           case {ub1: UnderBinder[_, _], ub2: UnderBinder[_, _]} => result == result && be1.alpha_==(be2)
           case {a, b} => result == result && a.equals(b)
         }; x
       })
    }*/

    def cloneElementWithSubst(subst: Map[NameElement, NameElement]) = element.gmap(new Mapper { def apply[t <% Mappable[t]](x :t): t = x match{
      case substable: NameElement if subst.contains(substable) => subst.get(substable).asInstanceOf[t] // TODO: wrong... substitution is not (necessarily) the identity function
         //Console.println("substed: "+substable+"-> "+subst.get(substable)+")");
      case x => x // Console.println("subst: "+x+"(keys: "+subst.keys+")");x
    }})

    // TODO
    def cloneElementNoBoundElements = element.gmap(new Mapper { def apply[t <% Mappable[t]](x :t): t = x match{
      case BoundElement(el, _) => new UnboundElement(el).asInstanceOf[t] // TODO: precision stuff
      case x => x
    }})

    def extract: elementT = cloneElementNoBoundElements
    def extract(subst: Map[NameElement, NameElement]): elementT = cloneElementWithSubst(subst)

    /** Get a string representation of element, normally we don't allow direct access to element, but just getting a string representation is ok. */
    def elementToString: String = element.toString
  }

  //SYB type class instances
  implicit def UnderBinderIsMappable[bt <: NameElement <% Mappable[bt], st <% Mappable[st]](ub: UnderBinder[bt, st]): Mappable[UnderBinder[bt, st]] =
    new Mappable[UnderBinder[bt, st]] {
      def gmap(f: Mapper): UnderBinder[bt, st] = UnderBinder(f(ub.scope), f(ub.element))
    }

  implicit def ScopeIsMappable[bt <: NameElement <% Mappable[bt]](scope: Scope[bt]): Mappable[Scope[bt]] =
    new Mappable[Scope[bt]] {
      def gmap(f: Mapper): Scope[bt] = { val newScope = new Scope[bt]()
        for(b <- scope) newScope.addBinder(f(b))
        newScope
      }
    }

  implicit def NameElementIsMappable(self: NameElement): Mappable[NameElement] = new Mappable[NameElement] {
    def gmap(f: Mapper): NameElement = self match {
      case BoundElement(el, scope) => BoundElement(f(el), f(scope))
      case _ => UserNameElementIsMappable(self).gmap(f)
    }
  }

  def UserNameElementIsMappable[t <: NameElement](self: t): Mappable[t]

  object UnderBinder {
    def apply[binderType <: NameElement, elementT <% Mappable[elementT]](scope: Scope[binderType], element: elementT) = new UnderBinder(scope, element)
    def unit[bt <: NameElement, elementT <% Mappable[elementT]](x: elementT) = UnderBinder(new Scope[bt](), x)
  }

  /** If a list of `UnderBinder`s all have the same scope, they can be turned in to an `UnderBinder`
   *  containing a list of the elements in the original `UnderBinder`.
   *
   *  The name `sequence` comes from the fact that this method's type is equal to the type of monadic sequence.
   *
   *  @note `!orig.isEmpty` implies `orig.forall(ub => ub.scope eq orig(0).scope)`
   *
   */
  def sequence[bt <: NameElement, st <% Mappable[st]](orig: List[UnderBinder[bt, st]]): UnderBinder[bt, List[st]] =
    if(orig.isEmpty) UnderBinder.unit(Nil)
    else UnderBinder(orig(0).scope, orig.map(_.element))

  // couldn't come up with a better name...
  def unsequence[bt <: NameElement, st <% Mappable[st]](orig: UnderBinder[bt, List[st]]): List[UnderBinder[bt, st]] =
    orig.element.map(sc => UnderBinder(orig.scope, sc))

  //TODO: more documentation
  /** An environment that maps a `NameElement` to the scope in which it is bound.
   *  This can be used to model scoping during parsing.
   *
   *  @note This class uses similar techniques as described by ''Burak Emir'' in
   *        [[http://library.epfl.ch/theses/?nr=3899 Object-oriented pattern matching]],
   *        but uses `==` instead of `eq`, thus types can't be unified in general.
   */
  abstract class BinderEnv {
    def apply[A <: NameElement](v: A): Option[Scope[A]]
    def extend[a <: NameElement](v : a, x : Scope[a]) = new BinderEnv {
      def apply[b <: NameElement](w : b): Option[Scope[b]] =
        if(w == v) Some(x.asInstanceOf[Scope[b]])
        else BinderEnv.this.apply(w)
    }
  }

  object EmptyBinderEnv extends BinderEnv {
    def apply[A <: NameElement](v: A): Option[Scope[A]] = None
  }

  // TODO: move this to some utility object higher in the scala hierarchy?
  /** Returns a given result, but executes the supplied closure before returning.
   *  (The effect of this closure does not influence the returned value.)
   */
  trait ReturnAndDo[T]{
    /**
     *  @param block  code to be executed, purely for its side-effects
     */
    def andDo(block: => Unit): T
  }

  def return_[T](result: T): ReturnAndDo[T] =
    new ReturnAndDo[T] {
      val r = result
      def andDo(block: => Unit): T = {block; r}
    }

  private object _Binder {
    private var currentId = 0
    private[Binders] def genId = return_(currentId) andDo {currentId=currentId+1}
  }
}
