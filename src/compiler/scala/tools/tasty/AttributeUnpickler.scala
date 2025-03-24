/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.tasty

import scala.collection.immutable.BitSet

import scala.tools.tasty.{TastyFormat, TastyReader}
import TastyFormat.{isBooleanAttrTag, isStringAttrTag}

object AttributeUnpickler {

  /** Unpickle the `Attributes` section of a TASTy file. */
  def attributes(reader: TastyReader): Attributes = {
    import reader._

    val booleanTags = BitSet.newBuilder

    var lastTag = -1
    while (!isAtEnd) {
      val tag = readByte()
      if (isBooleanAttrTag(tag))
        booleanTags += tag
      else if (isStringAttrTag(tag)) {
        // read a name ref, which is the discarded UTF8 string value of the attribute.
        // in the future, if we need this value then look it up in the name table.
        val _ = readNameRef()
      }
      else
        assert(false, "unknown attribute tag: " + tag)

      assert(tag != lastTag, s"duplicate attribute tag: $tag")
      assert(tag > lastTag, s"attribute tags are not ordered: $tag after $lastTag")
      lastTag = tag
    }

    val isJava = booleanTags.result().contains(TastyFormat.JAVAattr)
    if (isJava) Attributes.javaSource else Attributes.empty
  }
}
