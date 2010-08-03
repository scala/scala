package scala

import annotation.target._

/**
 * An annotation that designates the name of the parameter to which it is
 * applied as deprecated. Using that name in a named argument generates
 * a deprecation warning.
 *
 * @since 2.8.1
 */
@param
class deprecatedName(name: Symbol) extends StaticAnnotation
