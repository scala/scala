/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
*/

// $Id$

package scala;

/** Temporary class when this appears in the atribute list of an abstract class, the
 *  class is assumed to be a trait. Used to ensure that cold that compiles under old scalac
 *  can also copmpile under nsc.
 */
class _trait_ extends Attribute {}
