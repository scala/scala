/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package generic

import language.higherKinds

/** A template for companion objects of IndexedSeq and subclasses thereof.
 *
 *  @since 2.11
 */
abstract class IndexedSeqFactory[CC[X] <: IndexedSeq[X] with GenericTraversableTemplate[X, CC]] extends SeqFactory[CC]
