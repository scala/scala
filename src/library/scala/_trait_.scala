/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
*/

// $Id:_trait_.scala 5359 2005-12-16 16:33:49 +0100 (Fri, 16 Dec 2005) dubochet $

package scala;

/** Temporary class.
 *  When this appears in the attribute list of an abstract class, the class
 *  is assumed to be a trait. Used to ensure that code that compiles under
 *  (old) <code>scalac</code> can also compile under <code>nsc</code>.
 */
class _trait_ extends Attribute {}
