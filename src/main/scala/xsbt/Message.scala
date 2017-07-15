/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import java.util.function.Supplier

object Message {
  def apply[T](s: => T): Supplier[T] = new Supplier[T] {
    override def get(): T = s
  }
}
