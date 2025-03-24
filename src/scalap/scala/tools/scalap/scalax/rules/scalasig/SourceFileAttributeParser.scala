/*
 * Scala classfile decoder (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.scalap
package scalax
package rules
package scalasig

/**
 * @author ilyas
 */

object SourceFileAttributeParser extends ByteCodeReader {
  val sourceFile = u2 ^^ SourceFileInfo

  def parse(byteCode: ByteCode) = expect(sourceFile)(byteCode)
}

/**
 *
 * SourceFile_attribute {
    	u2 attribute_name_index;
    	u4 attribute_length;
    	u2 sourcefile_index;
    }
 *
 * Contains only file index in ConstantPool, first two fields are already treated
 * by {@link scalax.rules.scalasig.ClassFileParser#attribute}
 */
case class SourceFileInfo(sourceFileIndex: Int)
