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

package scala.tools.tasty

import java.util.UUID

import TastyFormat.{MajorVersion, MinorVersion, header}

class TastyHeaderUnpickler(reader: TastyReader) {
  import reader._

  def this(bytes: Array[Byte]) = this(new TastyReader(bytes))

  def readHeader(): UUID = {
    for (i <- header.indices)
      check(readByte() == header(i), "not a TASTy file")
    val major = readNat()
    val minor = readNat()
    check(major == MajorVersion && minor <= MinorVersion,
      s"""TASTy signature has wrong version.
         | expected: $MajorVersion.$MinorVersion
         | found   : $major.$minor""".stripMargin)
    new UUID(readUncompressedLong(), readUncompressedLong())
  }

  def isAtEnd: Boolean = reader.isAtEnd

  private def check(cond: Boolean, msg: => String): Unit =
    if (!cond) throw new UnpickleException(msg)
}
