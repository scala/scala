/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect.internal

import java.io.{ DataInput, InputStream, DataInputStream, ByteArrayInputStream, BufferedInputStream, FileInputStream }
import scala.tools.nsc.io.{ Directory }
import scala.reflect.NameTransformer.decode
import scala.tools.util.StringOps.trimTrailingSpace
import ConstantPool._

final case class JvmVersion(minorVersion: Int, majorVersion: Int)

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
    assert(u4 == 0xCAFEBABE, "Bad magic number")
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

class JvmClassInfoBuilder(protected[this] val in: DataInput) extends ScalacClassfileModel {
  def readInterface(): InterfaceInfo      = nameAt(u2)
  def readMember(): JvmMemberInfo         = new JvmMemberInfo(u2.toShort, stringAt(u2), stringAt(u2), readAttributes())
  def readInnerClass(): JvmInnerClassInfo = new JvmInnerClassInfo(thisClass, nameAt(u2), nameAt(u2), stringAt(u2), u2.toShort)

  def readConstantPoolEntry(): Entry = (u1: @annotation.switch) match {
    case CONSTANT_Utf8               => new Utf8_info(in.readUTF)
    case CONSTANT_Integer            => new Integer_info(in.readInt)
    case CONSTANT_Float              => new Float_info(in.readFloat)
    case CONSTANT_Long               => new Long_info(in.readLong)
    case CONSTANT_Double             => new Double_info(in.readDouble)
    case CONSTANT_Class              => new Class_info(u2)
    case CONSTANT_String             => new String_info(u2)
    case CONSTANT_Fieldref           => new Fieldref_info(u2, u2)
    case CONSTANT_Methodref          => new Methodref_info(u2, u2)
    case CONSTANT_InterfaceMethodref => new InterfaceMethodref_info(u2, u2)
    case CONSTANT_NameAndType        => new NameAndType_info(u2, u2)
  }

  // field_info attributes:
  // ConstantValue (§4.7.2), Synthetic (§4.7.8), Signature (§4.7.9), Deprecated (§4.7.15),
  // RuntimeVisibleAnnotations (§4.7.16) and RuntimeInvisibleAnnotations (§4.7.17).
  //
  // method_info attributes:
  // Code (§4.7.3), Exceptions (§4.7.5), Synthetic (§4.7.8), Signature (§4.7.9), Deprecated (§4.7.15),
  // RuntimeVisibleAnnotations (§4.7.16), RuntimeInvisibleAnnotations (§4.7.17), RuntimeVisibleParameterAnnotations (§4.7.18),
  // RuntimeInvisibleParameterAnnotations (§4.7.19) and AnnotationDefault (§4.7.20).

  def readAttribute(): AttributeInfo = stringAt(u2) match {
    case "Signature"    => u4 ; new SignatureAttr(stringAt(u2))
    case "InnerClasses" => u4 ; new InnerClassesAttr(readInnerClasses())
    case name           => val bytes = new Array[Byte](u4) ; in.readFully(bytes) ; new GenericAttr(name, bytes)
  }
}

object Classify {

  /*


  4.2.2 Unqualified Names

Names of methods, fields and local variables are stored as unqualified
names. Unqualified names must not contain the characters '.', ';', '['
or '/'. Method names are further constrained so that, with the exception
of the special method names <init> and <clinit> (§3.9), they must not
contain the characters '<' or '>'.

  4.3 Descriptors and Signatures

A descriptor is a string representing the type of a field or method.
Descriptors are represented in the class file format using modified
UTF-8 strings (§4.4.7) and thus may be drawn, where not further
constrained, from the entire Unicode character set. A signature is a
string representing the generic type of a field or method, or generic
type information for a class declaration.
*/

}

object ConstantPool {
  type UShort = Char

  final val CONSTANT_Utf8               = 1
  final val CONSTANT_Integer            = 3
  final val CONSTANT_Float              = 4
  final val CONSTANT_Long               = 5
  final val CONSTANT_Double             = 6
  final val CONSTANT_Class              = 7
  final val CONSTANT_String             = 8
  final val CONSTANT_Fieldref           = 9
  final val CONSTANT_Methodref          = 10
  final val CONSTANT_InterfaceMethodref = 11
  final val CONSTANT_NameAndType        = 12

  abstract class Name_Info(tag: Byte) extends PoolEntry(tag) {
    def name_index: UShort
  }
  abstract class Ref_Info(tag: Byte) extends PoolEntry(tag) {
    def class_index: UShort
    def name_and_type_index: UShort
  }
  class Class_info(val name_index: UShort) extends Name_Info(CONSTANT_Class) { }
  class Double_info(val value: Double) extends PoolEntry(CONSTANT_Double) {
    override def width = 2
  }
  class Fieldref_info(val class_index: UShort, val name_and_type_index: UShort) extends Ref_Info(CONSTANT_Fieldref)
  class Float_info(val value: Float) extends PoolEntry(CONSTANT_Float)
  class Integer_info(val value: Int) extends PoolEntry(CONSTANT_Integer)
  class InterfaceMethodref_info(val class_index: UShort, val name_and_type_index: UShort) extends Ref_Info(CONSTANT_InterfaceMethodref)
  class Long_info(val value: Long) extends PoolEntry(CONSTANT_Long) {
    override def width = 2
  }
  class Methodref_info(val class_index: UShort, val name_and_type_index: UShort) extends Ref_Info(CONSTANT_Methodref)
  class NameAndType_info(val name_index: UShort, val descriptor_index: UShort) extends Name_Info(CONSTANT_NameAndType) {
    override def toString = "NameAndType  #%s:#%s;".format(name_index, descriptor_index)
  }
  class String_info(val string_index: UShort) extends PoolEntry(CONSTANT_String) { }
  class Utf8_info(override val stringValue: String) extends PoolEntry(CONSTANT_Utf8) {
    override def toString = ("Asciz    " + stringValue).trim
  }

  abstract class PoolEntry(tag: Byte) {
    def width = 1
    def stringValue: String = sys.error("Not a String-valued constant pool entry: " + this)
    override def toString = (
      getClass.getName.split("[.$]").last + "/" + tag
    )
  }
  object NoEntry extends PoolEntry(-1) { }
}

abstract class JvmInfo(attributes: Array[JvmAttributeInfo]) {
  // def flags: Short
  def name: String

  val signature    = attributes collectFirst { case x: SignatureAttr => x.value } getOrElse ""
  val innerClasses = attributes collectFirst { case x: InnerClassesAttr => x.value } getOrElse Array()
}


class JvmClassInfo(
  val name: String,
  val superName: String,
  val interfaces: Array[String],
  val fields: Array[JvmMemberInfo],
  val methods: Array[JvmMemberInfo],
  attributes: Array[JvmAttributeInfo]
) extends JvmInfo(attributes) {

  def members           = fields ++ methods sortBy (_.decodedName)
  def memberDescriptors = members map (_.toErasedString)
  def memberSignatures  = members filter (_.hasSignature) map (_.toGenericString)
  def descriptorsString = if (memberDescriptors.nonEmpty) memberDescriptors.mkString("\n-- Member Descriptors --\n", "\n", "\n") else ""
  def signaturesString  = if (memberSignatures.nonEmpty) memberSignatures.mkString("\n-- Member Signatures --\n", "\n", "\n") else ""
  def innersString      = if (innerClasses.isEmpty) "" else innerClasses.mkString("\n-- Inner Classes --\n", "\n", "\n")
  def membersString     = descriptorsString + signaturesString
  def extendsString     = if (superName == "") "" else " extends " + superName
  def implementsString  = if (interfaces.isEmpty) "" else interfaces.mkString("Implements: ", ", ", "")

  private def group(label: String, xs: Traversable[(String, String)]) =
    xs map { case (name, value) => line(label, name, value) } mkString "\n"

  private def line(label: String, name: String, data: String) =
    trimTrailingSpace("  %-15s  %30s  %s".format(label, name, data))

  override def toString = (
    List(
      "class " + name + extendsString,
      if (signature == "") "" else line("class sig", "", signature),
      group("interface", interfaces map (x => (("", x)))),
      (innerClasses map (ic => line(ic.kind, ic.innerName, ic.nestString))).sorted.mkString("\n"),
      group("descriptor", members map (x => (x.name, x.descriptor))),
      group("signature", members filter (_.hasSignature) map (x => (x.name, x.signature)))
    ) map trimTrailingSpace filterNot (_ == "") mkString ("", "\n", "\n")
  )
}

// method_info or field_info {
//   u2 access_flags;
//   u2 name_index;
//   u2 descriptor_index;
//   u2 attributes_count;
//   attribute_info attributes[attributes_count];
// }
class JvmMemberInfo(
  val flags: Short,
  val name: String,
  val descriptor: String,
  attributes: Array[JvmAttributeInfo]
) extends JvmInfo(attributes) {
  def decodedName     = decode(name)
  def hasSignature    = signature != ""
  def toErasedString  = "%-30s %s".format(decodedName, descriptor)
  def toGenericString = "%-30s %s".format(decodedName, signature)

  override def toString = (
    if (hasSignature) toGenericString else toErasedString
  )
}

abstract class JvmAttributeInfo {
  def name: String
  def value: Any
}
class GenericAttr(val name: String, val value: Array[Byte]) extends JvmAttributeInfo {
  // attribute_info {
  //   u2 attribute_name_index;
  //   u4 attribute_length;
  //   u1 info[attribute_length];
  // }
}
class SignatureAttr(val value: String) extends JvmAttributeInfo {
  def name = "Signature"
}
class InnerClassesAttr(val value: Array[JvmInnerClassInfo]) extends JvmAttributeInfo {
  def name = "InnerClasses"
}

// package foo { class Foo { class Bar } }
//
// javap would say
//    Bar = class foo.Foo$Bar of class foo.Foo
// which is translated as
//   innerClass = foo.Foo$Bar
//   outerClass = foo.Foo
//    innerName = Bar

class JvmInnerClassInfo(
  thisClass: String,        // classfile which is being parsed
  val innerClass: String,   // the full name of the inner/nested class
  val outerClass: String,   // the full name of the outer class - must be a prefix of innerClass
  val innerName: String,    // the simple name of the inner class - should (?) be a suffix of innerClass
  val flags: Short          // flags
) {
  val isEntryOfEnclosingClass = !isAnonymousClass && (innerClass == thisClass)
  val isEntryOfNestedClass    = !isAnonymousClass && (outerClass == thisClass)

  def isTopLevelClass  = outerClass == ""
  def isAnonymousClass = innerName == ""
  def isMemberClass    = !isTopLevelClass

  def kind = (
    if (isEntryOfEnclosingClass) "inner/enclosing"
    else if (isEntryOfNestedClass) "inner/nested"
    else if (isAnonymousClass) "inner/anon"
    else "inner"
  )
  def nestString = (
    if (isEntryOfEnclosingClass) "enclosing class: " + outerClass
    else if (isEntryOfNestedClass) "member class: " + innerClass
    else if (isAnonymousClass) "anonymous class: " + innerClass
    else innerClass + " in " + outerClass
  )
  override def toString = innerName + "=" + nestString
}

object JvmClassInfo {
  private def classFiles(path: String) =
    Directory(path).deepFiles filter (_ hasExtension "class")

  def classInfoMap(path: String): Map[String, JvmClassInfo] = {
    classFiles(path) map (f => (f.path, JvmClassInfo fromFile f.jfile)) toMap
  }
  def classInfoList(path: String): List[(String, JvmClassInfo)] = {
    classInfoMap(path).toList sortBy (_._1)
  }
  
  def fromFile(file: java.io.File) =
    fromStream(new BufferedInputStream(new FileInputStream(file)))

  def fromBytes(bytes: Array[Byte]) =
    fromStream(new ByteArrayInputStream(bytes))

  def fromPath(path: String) =
    fromStream(new BufferedInputStream(new FileInputStream(path)))

  def fromStream(in0: InputStream) = {
    val in = new DataInputStream(in0)
    try fromDataInput(in) finally in.close()
  }

  def fromDataInput(in: DataInput): JvmClassInfo = {
    new JvmClassInfoBuilder(in) parse
  }
}
