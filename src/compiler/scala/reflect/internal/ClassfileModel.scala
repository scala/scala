/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect.internal

import java.io.DataInput
import ConstantPool._
import ClassfileConstants._

trait ClassfileModel {
  type Result
  type Entry
  type InterfaceInfo
  type MemberInfo
  type AttributeInfo
  type InnerClassInfo

  protected implicit def EntryArrayTag: ArrayTag[Entry]
  protected implicit def InterfaceInfoArrayTag: ArrayTag[InterfaceInfo]
  protected implicit def MemberInfoArrayTag: ArrayTag[MemberInfo]
  protected implicit def AttributeInfoArrayTag: ArrayTag[AttributeInfo]
  protected implicit def InnerClassInfoArrayTag: ArrayTag[InnerClassInfo]

  // These could be implemented to jump forward in the stream if the
  // result is not wanted.
  def readConstantPoolEntry(): Entry
  def readInterface(): InterfaceInfo
  def readMember(): MemberInfo
  def readAttribute(): AttributeInfo
  def readInnerClass(): InnerClassInfo

  def createInfo(
    version: JvmVersion,
    entries: Array[Entry],
    flags: Int,
    name: String,
    superName: String,
    interfaces: Array[InterfaceInfo],
    fields: Array[MemberInfo],
    methods: Array[MemberInfo],
    attributes: Array[AttributeInfo]
  ): Result
}

abstract class StreamingClassfileModel extends ClassfileModel {
  protected[this] val in: DataInput
  private[this] var name: String = _
  private[this] var entries: Array[PoolEntry] = _

  type Entry = PoolEntry

  // These translate null into "", it's less troublesome.
  protected def nameAt(idx: Int) = entries(idx) match {
    case x: Name_Info => stringAt(x.name_index).replace('/', '.')
    case _            => ""
  }
  protected def stringAt(idx: Int) = entries(idx) match {
    case x: Utf8_info => x.stringValue
    case _            => ""
  }

  protected def u4 = in.readInt
  protected def u2 = in.readUnsignedShort.toChar
  protected def u1 = in.readUnsignedByte

  // The constant_pool table is indexed from 1 to constant_pool_count−1.
  protected def readConstantPool(): Array[Entry] = {
    val count = u2
    val entries = new Array[Entry](count)
    var i = 1
    while (i < count) {
      val entry = readConstantPoolEntry()
      entries(i) = entry
      i += entry.width
    }
    entries
  }
  protected def readInterfaces() = {
    val count = u2
    val interfaces = new Array[InterfaceInfo](count)
    var i = 0
    while (i < count) {
      interfaces(i) = readInterface()
      i += 1
    }
    interfaces
  }
  protected def readMembers() = {
    val count = u2
    val arr = new Array[MemberInfo](count)
    var i = 0
    while (i < count) {
      arr(i) = readMember()
      i += 1
    }
    arr
  }
  protected def readAttributes(): Array[AttributeInfo] = {
    val count = u2
    val arr = new Array[AttributeInfo](count)
    var i = 0
    while (i < count) {
      arr(i) = readAttribute()
      i += 1
    }
    arr
  }
  protected def readInnerClasses() = {
    val count = u2
    val arr = new Array[InnerClassInfo](count)
    var i = 0
    while (i < count) {
      arr(i) = readInnerClass()
      i += 1
    }
    arr
  }
  protected def thisClass = name

  def parse() = {
    assert(u4 == JAVA_MAGIC, "Bad magic number")
    val version    = JvmVersion(u2, u2)
    this.entries   = readConstantPool()
    val flags      = u2.toShort
    this.name      = nameAt(u2)
    val superName  = nameAt(u2)
    val interfaces = readInterfaces()
    val fields     = readMembers()
    val methods    = readMembers()
    val attributes = readAttributes()

    try createInfo(version, entries, flags, name, superName, interfaces, fields, methods, attributes)
    finally entries = null
  }
}

abstract class ScalacClassfileModel extends StreamingClassfileModel {
  type Result         = JvmClassInfo
  type InterfaceInfo  = String
  type MemberInfo     = JvmMemberInfo
  type AttributeInfo  = JvmAttributeInfo
  type InnerClassInfo = JvmInnerClassInfo

  protected implicit def EntryArrayTag = arrayTag[PoolEntry]
  protected implicit def InterfaceInfoArrayTag = arrayTag[InterfaceInfo]
  protected implicit def MemberInfoArrayTag = arrayTag[MemberInfo]
  protected implicit def AttributeInfoArrayTag = arrayTag[AttributeInfo]
  protected implicit def InnerClassInfoArrayTag = arrayTag[InnerClassInfo]

  def readConstantPoolEntry(): PoolEntry
  def readInterface(): String
  def readMember(): JvmMemberInfo
  def readAttribute(): JvmAttributeInfo
  def readInnerClass(): JvmInnerClassInfo

  def createInfo(
    version: JvmVersion,
    entries: Array[PoolEntry],
    flags: Int,
    name: String,
    superName: String,
    interfaces: Array[String],
    fields: Array[JvmMemberInfo],
    methods: Array[JvmMemberInfo],
    attributes: Array[JvmAttributeInfo]
  ): JvmClassInfo = new JvmClassInfo(name, superName, interfaces, fields, methods, attributes)
}
