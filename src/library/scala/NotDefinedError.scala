/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
**                                                                      **
** $Id: MatchError.scala 5390 2005-12-19 13:49:03Z dubochet $
\*                                                                      */
package scala;

final class NotDefinedError(msg: String) extends Error("not defined: " + msg);
