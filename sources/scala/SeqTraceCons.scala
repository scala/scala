/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2003, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

/** nonempty SeqTrace
  */
final case class SeqTraceCons[A](hdI: Int, hdb: A, tl: SeqTrace[A])
    extends SeqTrace[A] {

    def isEmpty = false;

    def head = Pair(hdI, hdb);

    def headState = hdI;

    def headElem  = hdb;

    def tail:SeqTrace[A] = tl;

    //override def toString(): String = mkString2("[ ", "; ", " ]");

}
