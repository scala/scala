/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */
package scala.xml ;

object Parsing {

  /** (#x20 | #x9 | #xD | #xA) */
  final def isSpace( ch:Char ):Boolean = ch match {
    case '\u0009' | '\u000A' | '\u000D' | '\u0020' => true
    case _                                         => false;
  }

  /** (#x20 | #x9 | #xD | #xA)+ */
  final def isSpace( cs:Seq[Char] ):Boolean = {
    val it = cs.elements;
    it.hasNext && it.forall { isSpace };
  }
}
