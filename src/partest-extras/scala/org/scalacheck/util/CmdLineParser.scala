/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2014 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck.util

import scala.collection.Set
import org.scalacheck.Test

private[scalacheck] trait CmdLineParser {

  type Elem = String

  trait Opt[+T] {
    val default: T
    val names: Set[String]
    val help: String
  }
  trait Flag extends Opt[Unit]
  trait IntOpt extends Opt[Int]
  trait FloatOpt extends Opt[Float]
  trait StrOpt extends Opt[String]

  class OptMap {
    private val opts = new collection.mutable.HashMap[Opt[_], Any]
    def apply(flag: Flag): Boolean = opts.contains(flag)
    def apply[T](opt: Opt[T]): T = opts.get(opt) match {
      case None => opt.default
      case Some(v) => v.asInstanceOf[T]
    }
    def update[T](opt: Opt[T], optVal: T) = opts.update(opt, optVal)
  }

  val opts: Set[Opt[_]]

}
