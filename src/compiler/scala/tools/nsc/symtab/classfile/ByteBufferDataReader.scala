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

package scala.tools.nsc.symtab.classfile

import java.io.{DataInputStream, InputStream}
import java.nio.{BufferUnderflowException, ByteBuffer}


final class ByteBufferDataReader(data0: ByteBuffer) extends DataReader {
  private[this] var data = data0
  private[this] val stream = new InputStream {
    override def read(): Int = try {
      data.get & 0xff
    } catch {
      case _: BufferUnderflowException => -1
    }
    override def markSupported(): Boolean = false
  }
  private[this] val reader = new DataInputStream(stream)
  @throws(classOf[IndexOutOfBoundsException])
  def nextByte: Byte = data.get

  def nextBytes(len: Int): Array[Byte] = {
    val result = new Array[Byte](len)
    reader.readFully(result)
    result
  }

  def nextChar: Char = data.getChar()

  def nextInt: Int = data.getInt()

  def getChar(mybp: Int): Char = {
    data.getChar(mybp)
  }

  def getInt(mybp: Int): Int = {
    data.getInt(mybp)
  }

  def getLong(mybp: Int): Long = {
    data.getLong(mybp)
  }

  def getFloat(mybp: Int): Float = {
    data.getFloat(mybp)
  }

  def getDouble(mybp: Int): Double = {
    data.getDouble(mybp)
  }

  def skip(n: Int): Unit = {
    data.position(data.position() + n)
  }
  def bp: Int = data.position()
  def bp_=(i: Int): Unit = data.position(i)

  def getByte(mybp: Int): Byte = {
    data.get(mybp)
  }
  def getBytes(mybp: Int, bytes: Array[Byte]): Unit = {
    val saved = data.position
    data.position(mybp)
    try reader.readFully(bytes)
    finally data.position(saved)
  }
  def getUTF(mybp: Int, len: Int): String = {
    val saved = data.position
    val savedLimit = data.limit()
    data.position(mybp)
    data.limit(mybp + len)
    try reader.readUTF()
    finally {
      data.limit(savedLimit)
      data.position(saved)
    }
  }
}
