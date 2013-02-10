package scala.reflect
package internal
package annotations

import scala.annotation.meta._

/**
 * An annotation that designates a member should not be referred to after
 * type checking (which includes macro expansion); it must only be used in
 * the arguments of some other macro that will eliminate it from the AST.
 *
 * Later on, this annotation should be removed and implemented with domain-specific macros.
 * If a certain method `inner` mustn't be called outside the context of a given macro `outer`,
 * then it should itself be declared as a macro.
 *
 * Approach #1. Expansion of `inner` checks whether its enclosures contain `outer` and
 * report an error if `outer` is not detected. In principle, we could use this approach right now,
 * but currently enclosures are broken, because contexts aren't exactly famous for keeping precise
 * track of the stack of the trees being typechecked.
 *
 * Approach #2. Default implementation of `inner` is just an invocation of `c.abort`.
 * `outer` is an untyped macro, which expands into a block, which contains a redefinition of `inner`
 * and a call to itself. The redefined `inner` could either be a stub like `Expr.splice` or  carry out
 * domain-specific logic.
 *
 * @param  message the error message to print during compilation if a reference remains
 *                 after type checking
 * @since  2.10.1
 */
@getter @setter @beanGetter @beanSetter
final class compileTimeOnly(message: String) extends scala.annotation.StaticAnnotation
