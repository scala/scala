/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import scala.util.control.ControlThrowable
import symtab.Flags._
import scala.annotation.tailrec

/** On pattern matcher checkability:
 *
 *  Consider a pattern match of this form: (x: X) match { case _: P => }
 *
 *  There are four possibilities to consider:
 *     [P1] X will always conform to P
 *     [P2] x will never conform to P
 *     [P3] X <: P if some runtime test is true
 *     [P4] X cannot be checked against P
 *
 *  The first two cases correspond to those when there is enough static
 *  information to say X <: P or that !(X <: P) for all X and P.
 *  The fourth case includes unknown abstract types or structural
 *  refinements appearing within a pattern.
 *
 *  The third case is the interesting one.  We designate another type, XR,
 *  which is essentially the intersection of X and |P|, where |P| is
 *  the erasure of P.  If XR <: P, then no warning is emitted.
 *
 *  Examples of how this info is put to use:
 *  sealed trait A[T] ; class B[T] extends A[T]
 *    def f(x: B[Int]) = x match { case _: A[Int] if true => }
 *    def g(x: A[Int]) = x match { case _: B[Int] => }
 *
 *  `f` requires no warning because X=B[Int], P=A[Int], and B[Int] <:< A[Int].
 *  `g` requires no warning because X=A[Int], P=B[Int], XR=B[Int], and B[Int] <:< B[Int].
 *      XR=B[Int] because a value of type A[Int] which is tested to be a B can
 *      only be a B[Int], due to the definition of B (B[T] extends A[T].)
 *
 *  This is something like asSeenFrom, only rather than asking what a type looks
 *  like from the point of view of one of its base classes, we ask what it looks
 *  like from the point of view of one of its subclasses.
 */
trait Checkable {
  self: Analyzer =>

  import global._
  import definitions._

  trait InferCheckable {
    self: Inferencer =>

    import InferErrorGen._
    private def context = getContext

    // if top-level abstract types can be checked using a classtag extractor, don't warn about them
    def checkCheckable(tree: Tree, typeToTest: Type, typeEnsured: Type, inPattern: Boolean, canRemedy: Boolean = false) = {
      log(s"checkCheckable($tree, $typeToTest, $typeEnsured, inPattern = $inPattern, canRemedy = $canRemedy")

      sealed abstract class TypeConformance(check: (Type, Type) => Boolean) {
        def apply(t1: Type, t2: Type): Boolean = check(t1, t2) && {
          log(s"Skipping unchecked for statically verifiable condition $t1 ${this} $t2")
          true
        }
      }
      // I tried to use varianceInType to track the variance implications
      // but I could not make it work.
      case object =:= extends TypeConformance(_ =:= _)
      case object <:< extends TypeConformance(_ <:< _)
      case object >:> extends TypeConformance((t1, t2) => t2 <:< t1)
      case object =!= extends TypeConformance((t1, t2) => false)

      var bound: List[Symbol] = Nil
      var warningMessages: List[String] = Nil

      def isLocalBinding(sym: Symbol) = (
        sym.isAbstractType && (
             (bound contains sym)
          || (sym.name == tpnme.WILDCARD)
          || {
            val e = context.scope.lookupEntry(sym.name)
            (e ne null) && e.sym == sym && !e.sym.isTypeParameterOrSkolem && e.owner == context.scope
          }
        )
      )
      def check(tp0: Type, pt: Type, conformance: TypeConformance): Boolean = {
        val tp = tp0.normalize
        // Set the warning message to be issued when the top-level call fails.
        def warn(what: String): Boolean = {
          warningMessages ::= what
          false
        }
        def checkArg(param: Symbol, arg: Type) = {
          def conforms = (
            if (param.isCovariant) <:<
            else if (param.isContravariant) >:>
            else =:=
          )
          (arg hasAnnotation UncheckedClass) || {
            arg.withoutAnnotations match {
              case TypeRef(_, sym, args) =>
                (    isLocalBinding(sym)
                  || arg.typeSymbol.isTypeParameterOrSkolem
                  || (sym.name == tpnme.WILDCARD) // avoid spurious warnings on HK types
                  || check(arg, param.tpeHK, conforms)
                  || warn("non-variable type argument " + arg)
                )
              case _ =>
                warn("non-variable type argument " + arg)
            }
          }
        }

        // Checking if pt (the expected type of the pattern, and the type
        // we are guaranteed) conforms to tp (the type expressed in the pattern's
        // type test.) If it does, then even if the type being checked for appears
        // to be uncheckable, it is not a warning situation, because it is indeed
        // checked: not at runtime, but statically.
        conformance.apply(pt, tp) || (tp match {
          case SingleType(pre, _)                           => check(pre, pt, =:=)
          case ExistentialType(quantified, tp1)             => bound :::= quantified ; check(tp1, pt, <:<)
          case ThisType(_) | NoPrefix                       => true
          case RefinedType(parents, decls) if decls.isEmpty => parents forall (p => check(p, pt, <:<))
          case RefinedType(_, _)                            => warn("refinement " + tp)
          case TypeRef(_, ArrayClass, arg :: Nil)           => check(arg, NoType, =!=)
          case TypeRef(_, NonLocalReturnControlClass, _)    => true // no way to suppress unchecked warnings on try/catch
          // we only use the extractor for top-level type tests, type arguments remain unchecked
          case TypeRef(_, sym, _) if sym.isAbstractType     => isLocalBinding(sym) || canRemedy || warn("abstract type " + tp)
          case TypeRef(_, _, Nil)                           => false // leaf node
          case TypeRef(pre, sym, args)                      => forall2(sym.typeParams, args)(checkArg) && check(pre, pt.prefix, =:=)
          case _                                            => warn("type " + tp)
        })
      }
      typeToTest match {
        // Prohibit top-level type tests for these, but they are
        // acceptable nested (e.g. case Foldable[Nothing] => ... )
        case TypeRef(_, NothingClass | NullClass | AnyValClass, _) =>
          TypePatternOrIsInstanceTestError(tree, typeToTest)
        case _ =>
          def where = ( if (inPattern) "pattern " else "" ) + typeToTest
          if (check(typeToTest, typeEnsured, =:=)) ()
          // Note that this is a regular warning, not an uncheckedWarning,
          // which is now the province of such notifications as "pattern matcher
          // exceeded its analysis budget."
          else warningMessages foreach (m =>
            context.unit.warning(tree.pos, s"$m in type $where is unchecked since it is eliminated by erasure"))
      }
    }
  }
}
