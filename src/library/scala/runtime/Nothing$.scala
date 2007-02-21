/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Nothing$.scala 9261 2006-11-14 17:11:16 +0000 (Tue, 14 Nov 2006) mihaylov $


package scala.runtime


/**
 * Dummy class which exist only to satisfy the JVM. It corresponds
 * to <code>scala.Nothing</code>. If such type appears in method
 * signatures, it is erased to this one.
 */

sealed abstract class Nothing$
