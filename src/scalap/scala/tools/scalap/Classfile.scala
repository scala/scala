/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003-2006, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/
**
*/

// $Id$

package scala.tools.scalap


class Classfile(in: ByteArrayReader) {
  import Classfiles._

  assert(in.nextInt == JAVA_MAGIC)
  val minorVersion = in.nextChar
  val majorVersion = in.nextChar
  val pool = readPool
  val flags = in.nextChar
  val classname = in.nextChar
  val superclass = in.nextChar
  val interfaces = readInterfaces
  val fields = readMembers(true)
  val methods = readMembers(false)
  val attribs = readAttribs

  def readAttribs = {
    val n = in.nextChar
    var attribs: List[Attribute] = Nil
    var i = 0
    while (i < n) {
      attribs = Attribute(in.nextChar, in.nextBytes(in.nextInt)) :: attribs
      i = i + 1
    }
    attribs
  }

  def readMembers(field: Boolean) = {
    val n = in.nextChar
    var members: List[Member] = Nil
    var i = 0
    while (i < n) {
      members = Member(field, in.nextChar, in.nextChar, in.nextChar, readAttribs) :: members
      i = i + 1
    }
    members
  }

  def readInterfaces = {
    val n = in.nextChar
    var intfs: List[Int] = Nil
    var i = 0
    while (i < n) {
      intfs = in.nextChar :: intfs
      i = i + 1
    }
    intfs
  }

  def readPool = {
    val pool = new Array[PoolEntry](in.nextChar)
    var i = 1
    while (i < pool.length) {
      val tag: Int = in.nextByte
      tag match {
        case CONSTANT_UTF8 =>
          pool(i) = UTF8(in.nextUTF8(in.nextChar))
        case CONSTANT_UNICODE =>
          in.skip(in.nextChar)
          pool(i) = Empty()
        case CONSTANT_CLASS =>
          pool(i) = ClassRef(in.nextChar)
        case CONSTANT_STRING =>
          pool(i) = StringConst(in.nextChar)
        case CONSTANT_FIELDREF =>
          pool(i) = FieldRef(in.nextChar, in.nextChar)
        case CONSTANT_METHODREF =>
          pool(i) = MethodRef(in.nextChar, in.nextChar)
        case CONSTANT_INTFMETHODREF =>
          pool(i) = IntfMethodRef(in.nextChar, in.nextChar)
        case CONSTANT_NAMEANDTYPE =>
          pool(i) = NameAndType(in.nextChar, in.nextChar)
        case CONSTANT_INTEGER =>
          pool(i) = IntegerConst(in.nextInt)
        case CONSTANT_FLOAT =>
          pool(i) = FloatConst(in.nextFloat)
        case CONSTANT_LONG =>
          pool(i) = LongConst(in.nextLong)
          i = i + 1
          pool(i) = Empty()
        case CONSTANT_DOUBLE =>
          pool(i) = DoubleConst(in.nextDouble)
          i = i + 1
          pool(i) = Empty()
      }
      i = i + 1
    }
    pool
  }

  class PoolEntry
  case class UTF8(str: String) extends PoolEntry
  case class ClassRef(classId: Int) extends PoolEntry
  case class FieldRef(classId: Int, memberId: Int) extends PoolEntry
  case class MethodRef(classId: Int, memberId: Int) extends PoolEntry
  case class IntfMethodRef(classId: Int, memberId: Int) extends PoolEntry
  case class StringConst(strId: Int) extends PoolEntry
  case class IntegerConst(x: Int) extends PoolEntry
  case class FloatConst(x: Float) extends PoolEntry
  case class LongConst(x: Long) extends PoolEntry
  case class DoubleConst(x: Double) extends PoolEntry
  case class NameAndType(nameId: Int, typeId: Int) extends PoolEntry
  case class Empty() extends PoolEntry

  case class Member(field: Boolean, flags: Int, name: Int, tpe: Int, attribs: List[Attribute])
  case class Attribute(name: Int, data: Array[Byte]) {

    override def toString(): String = pool(name) match {
      case UTF8(str: String) => str
    }

    def reader: ByteArrayReader = new ByteArrayReader(data)
  }

}
