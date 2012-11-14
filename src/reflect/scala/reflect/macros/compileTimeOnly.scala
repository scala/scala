package scala.reflect
package macros

import scala.annotation.meta._

/**
 * An annotation that designates a member should not be referred to after
 * type checking (which includes macro expansion); it must only be used in
 * the arguments of some other macro that will eliminate it from the AST.
 *
 * @param  message the error message to print during compilation if a reference remains
 *                 after type checking
 * @since  2.10.1
 */
@getter @setter @beanGetter @beanSetter
final class compileTimeOnly(message: String) extends scala.annotation.StaticAnnotation
