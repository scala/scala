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


class Classfile(in: ByteArrayReader) {
  import Classfiles._

  type UTF8 = Pool#UTF8

  assert(in.nextInt == JAVA_MAGIC)
  val minorVersion = in.nextChar
  val majorVersion = in.nextChar
  val pool = new Pool()
  val flags = in.nextChar
  val classname = in.nextChar
  val superclass = in.nextChar
  val interfaces = readInterfaces
  val fields = readMembers(field = true)
  val methods = readMembers(field = false)
  val attribs = readAttribs
  def scalaSigAttribute = attribs.find(_.toString == Main.SCALA_SIG)

  def readAttribs = {
    val n = in.nextChar
    var attribs: List[Attribute] = Nil
    var i = 0
    while (i < n) {
      attribs = Attribute(in.nextChar.toInt, in.nextBytes(in.nextInt)) :: attribs
      i = i + 1
    }
    attribs
  }

  def readMembers(field: Boolean) = {
    val n = in.nextChar
    var members: List[Member] = Nil
    var i = 0
    while (i < n) {
      members = Member(field, in.nextChar.toInt, in.nextChar.toInt, in.nextChar.toInt, readAttribs) :: members
      i = i + 1
    }
    members
  }

  def readInterfaces = {
    val n = in.nextChar
    var intfs: List[Int] = Nil
    var i = 0
    while (i < n) {
      intfs = in.nextChar.toInt :: intfs
      i = i + 1
    }
    intfs
  }

  class Pool() {
    sealed abstract class PoolEntry(val tag: Int) {
      def typeString = constantTagToString(tag)
    }
    case class UTF8(str: String) extends PoolEntry(CONSTANT_UTF8) { override def toString = "\"" + str + "\"" }
    case class ClassRef(classId: Int) extends PoolEntry(CONSTANT_CLASS) { override def toString = "Class(%s)".format(entries(classId)) }
    case class FieldRef(classId: Int, memberId: Int) extends PoolEntry(CONSTANT_FIELDREF)
    case class MethodRef(classId: Int, memberId: Int) extends PoolEntry(CONSTANT_METHODREF) {
      // //Method java/lang/Object."<init>":()V
      override def toString() = "Method %s.\"%s\"".format(entries(classId), entries(memberId))
    }
    case class IntfMethodRef(classId: Int, memberId: Int) extends PoolEntry(CONSTANT_INTFMETHODREF)
    case class StringConst(strId: Int) extends PoolEntry(CONSTANT_STRING)
    case class IntegerConst(x: Int) extends PoolEntry(CONSTANT_INTEGER)
    case class FloatConst(x: Float) extends PoolEntry(CONSTANT_FLOAT)
    case class LongConst(x: Long) extends PoolEntry(CONSTANT_LONG)
    case class DoubleConst(x: Double) extends PoolEntry(CONSTANT_DOUBLE)
    case class NameAndType(nameId: Int, typeId: Int) extends PoolEntry(CONSTANT_NAMEANDTYPE)
    case object Empty extends PoolEntry(0) { }
    case class MethodHandle(kindId: Int, refId: Int) extends PoolEntry(CONSTANT_METHODHANDLE)
    case class InvokeDynamic(bootMethodId: Int, nameTypeId: Int) extends PoolEntry(CONSTANT_INVDYNAMIC)
    case class MethodType(descId: Int) extends PoolEntry(CONSTANT_METHODTYPE)

    val entries = {
      val pool = new Array[PoolEntry](in.nextChar.toInt)
      var i = 1
      while (i < pool.length) {
        val tag = in.nextByte
        // Double sized entry
        if (tag == CONSTANT_LONG || tag == CONSTANT_DOUBLE) {
          pool(i) = if (tag == CONSTANT_LONG) LongConst(in.nextLong) else DoubleConst(in.nextDouble)
          i = i + 1
          pool(i) = Empty
        }
        else pool(i) = tag match {
          case CONSTANT_UTF8            => UTF8(in.nextUTF8(in.nextChar.toInt))
          case CONSTANT_UNICODE         => in.skip(in.nextChar) ; Empty
          case CONSTANT_CLASS           => ClassRef(in.nextChar)
          case CONSTANT_STRING          => StringConst(in.nextChar)
          case CONSTANT_FIELDREF        => FieldRef(in.nextChar, in.nextChar)
          case CONSTANT_METHODREF       => MethodRef(in.nextChar, in.nextChar)
          case CONSTANT_INTFMETHODREF   => IntfMethodRef(in.nextChar, in.nextChar)
          case CONSTANT_NAMEANDTYPE     => NameAndType(in.nextChar, in.nextChar)
          case CONSTANT_INTEGER         => IntegerConst(in.nextInt)
          case CONSTANT_FLOAT           => FloatConst(in.nextFloat)
          case CONSTANT_METHODHANDLE    => MethodHandle(in.nextByte, in.nextChar)
          case CONSTANT_METHODTYPE      => MethodType(in.nextChar)
          case CONSTANT_INVDYNAMIC      => InvokeDynamic(in.nextChar, in.nextChar)
        }

        i += 1
      }
      pool
    }

    lazy val length = entries.length
    def apply(x: Int) = entries(x)
    def stringOf(x: Int) = apply(x).toString
    override def toString = (
      for ((x, i) <- entries.zipWithIndex ; if x != null) yield
        "const #%d = %s\t%s\n".format(i + 1, x.typeString, x)
    ).mkString
  }

  /** **/
  case class Member(field: Boolean, flags: Int, name: Int, tpe: Int, attribs: List[Attribute])
  case class Attribute(name: Int, data: Array[Byte]) {
    override def toString = (pool(name): @unchecked) match {
      case pool.UTF8(s) => s
    }
    def reader: ByteArrayReader = new ByteArrayReader(data)
  }
}
