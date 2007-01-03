/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: remote.scala 9400 2006-11-28 17:22:45 +0000 (Tue, 28 Nov 2006) michelou $


package scala

/**
 * An attribute that designates the definition to which it is applied as deprecated.
 * Access to the member then generates a deprecated warning.
 */
class deprecated extends Attribute {}
