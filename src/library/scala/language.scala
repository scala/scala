package scala

object language {

  import languageFeature._

  /** Where enabled, direct or indirect subclasses of trait scala.Dynamic can
   *  be defined. Unless dynamics is enabled, a definition of a class, trait,
   *  or object that has Dynamic as a base trait is rejected. Dynamic member
   *  selection of existing subclasses of trait Dynamic are unaffected;
   *  they can be used anywhere.
   *
   *  _Why introduce the feature?_ To enable flexible DSLs and convenient interfacing
   *  with dynamic languages.
   *
   *  _Why control it?_ Dynamic member selection can undermine static checkability
   *  of programs. Furthermore, dynamic member selection often relies on reflection,
   *  which is not available on all platforms.
   */
  implicit lazy val dynamics: dynamics = languageFeature.dynamics

  /** Only where enabled, postfix operator notation `(expr op)` will be allowed.
   *
   *  _Why keep the feature?_ Several DSLs written in Scala need the notation.
   *
   *  _Why control it?_ Postfix operators interact poorly with semicolon inference.
   *   Most programmers avoid them for this reason.
   */
  implicit lazy val postfixOps: postfixOps = languageFeature.postfixOps

  /** Only where enabled, accesses to members of structural types that need
   *  reflection are supported. Reminder: A structural type is a type of the form 
   *  `Parents { Decls }` where `Decls` contains declarations of new members that do
   *  not override any member in `Parents`. To access one of these members, a
   *  reflective call is needed.
   *
   *  _Why keep the feature?_ Structural types provide great flexibility because
   *  they avoid the need to define inheritance hierarchies a priori. Besides,
   *  their definition falls out quite naturally from Scala’s concept of type refinement.
   *
   *  _Why control it?+ Reflection is not available on all platforms. Popular tools
   *  such as ProGuard have problems dealing with it. Even where reflection is available,
   *  reflective dispatch can lead to surprising performance degradations.
   */
  implicit lazy val reflectiveCalls: reflectiveCalls = languageFeature.reflectiveCalls

  /** Only where enabled, definitions of implicit conversions are allowed. An
   *  implicit conversion is an implicit value of unary function type `A => B`,
   *  or an implicit method that has in its first parameter section a single,
   *  non-implicit parameter. Examples:
   *
   *     implicit def stringToInt(s: String): Int = s.length
   *     implicit val conv = (s: String) => s.length
   *     implicit def listToX(xs: List[T])(implicit f: T => X): X = …
   * 
   *  implicit values of other types are not affected, and neither are implicit
   *  classes.
   *
   *  _Why keep the feature?_ Implicit conversions are central to many aspects
   *  of Scala’s core libraries.
   *
   *  _Why control it?_ Implicit conversions are known to cause many pitfalls
   *  if over-used. And there is a tendency to over-use them because they look
   *  very powerful and their effects seem to be easy to understand. Also, in
   *  most situations using implicit parameters leads to a better design than
   *  implicit conversions.
   */
  implicit lazy val implicitConversions: implicitConversions = languageFeature.implicitConversions

  /** Only where this flag is enabled, higher-kinded types can be written.
   *
   *  _Why keep the feature?_ Higher-kinded types enable the definition of very general
   *  abstractions such as functor, monad, or arrow. A significant set of advanced
   *  libraries relies on them. Higher-kinded types are also at the core of the
   *  scala-virtualized effort to produce high-performance parallel DSLs through staging.
   *
   *  _Why control it?_ Higher kinded types in Scala lead to a Turing-complete
   *  type system, where compiler termination is no longer guaranteed. They tend
   *  to be useful mostly for type-level computation and for highly generic design
   *  patterns. The level of abstraction implied by these design patterns is often
   *  a barrier to understanding for newcomers to a Scala codebase. Some syntactic
   *  aspects of higher-kinded types are hard to understand for the uninitiated and
   *  type inference is less effective for them than for normal types. Because we are
   *  not completely happy with them yet, it is possible that some aspects of
   *  higher-kinded types will change in future versions of Scala. So an explicit
   *  enabling also serves as a warning that code involving higher-kinded types
   *  might have to be slightly revised in the future.
   */
  implicit lazy val higherKinds: higherKinds = languageFeature.higherKinds

  /** Only where enabled, existential types that cannot be expressed as wildcard
   *  types can be written and are allowed in inferred types of values or return
   *  types of methods. Existential types with wildcard type syntax such as `List[_]`,
   *  or `Map[String, _]` are not affected.
   *
   *  _Why keep the feature?_ Existential types are needed to make sense of Java’s wildcard
   *  types and raw types and the erased types of run-time values.
   * 
   *  Why control it? Having complex existential types in a code base usually makes
   *  application code very brittle, with a tendency to produce type errors with
   *  obscure error messages. Therefore, going overboard with existential types
   *  is generally perceived not to be a good idea. Also, complicated existential types
   *  might be no longer supported in a future simplification of the language.
   */
  implicit lazy val existentials: existentials = languageFeature.existentials

  object experimental {

    import languageFeature.experimental._

    /** Where enabled, macro definitions are allowed. Macro implementations and
     *  macro applications are unaffected; they can be used anywhere.
     * 
     *  _Why introduce the feature?_ Macros promise to make the language more regular,
     *  replacing ad-hoc language constructs with a general powerful abstraction
     *  capability that can express them. Macros are also a more disciplined and
     *  powerful replacement for compiler plugins.
     *
     *  _Why control it?_ For their very power, macros can lead to code that is hard
     *  to debug and understand.
     */
    implicit lazy val macros: macros = languageFeature.experimental.macros
  }
}
