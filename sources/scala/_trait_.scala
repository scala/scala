/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
*/

// $Id$

package scala;

/** Temporary class.
 *  When this appears in the attribute list of an abstract class, the class
 *  is assumed to be a trait. Used to ensure that code that compiles under
 *  (old) <code>scalac</code> can also compile under <code>nsc</code>.
 */
class _trait_ extends Attribute {}
