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
import ClassfileConstants._

abstract class JvmInfo(attributes: Array[JvmAttributeInfo]) {
  // def flags: Short
  def name: String

  val signature    = attributes collectFirst { case x: SignatureAttr => x.value } getOrElse ""
  val innerClasses = attributes collectFirst { case x: InnerClassesAttr => x.value } getOrElse Array()
}
abstract class JvmAttributeInfo {
  // attribute_info {
  //   u2 attribute_name_index;
  //   u4 attribute_length;
  //   u1 info[attribute_length];
  // }
  def name: String
  def value: Any
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

  final def isAbstract  = (flags & JAVA_ACC_ABSTRACT) != 0
  final def isAnnotated = (flags & JAVA_ACC_ANNOTATION) != 0
  final def isFinal     = (flags & JAVA_ACC_FINAL) != 0
  final def isPrivate   = (flags & JAVA_ACC_PRIVATE) != 0
  final def isProtected = (flags & JAVA_ACC_PROTECTED) != 0
  final def isPublic    = (flags & JAVA_ACC_PUBLIC) != 0
  final def isStatic    = (flags & JAVA_ACC_STATIC) != 0
  final def isSynthetic = (flags & JAVA_ACC_SYNTHETIC) != 0
  final def isVarargs   = (flags & JAVA_ACC_VARARGS) != 0

  // method only
  final def isBridge = (flags & JAVA_ACC_BRIDGE) != 0

  // field only
  final def isEnum      = (flags & JAVA_ACC_ENUM) != 0
  final def isTransient = (flags & JAVA_ACC_TRANSIENT) != 0

  def isMethod        = descriptor startsWith "(" // )
  def isField         = !isMethod
  def scalaFlags      = toScalaMethodFlags(flags)
  def decodedName     = decode(name)
  def hasSignature    = signature != ""
  def toErasedString  = "%-30s %s".format(decodedName, descriptor)
  def toGenericString = "%-30s %s".format(decodedName, signature)

  override def toString = (
    if (hasSignature) toGenericString else toErasedString
  )
}
class GenericAttr(val name: String, val value: Array[Byte]) extends JvmAttributeInfo { }
class SignatureAttr(val value: String) extends JvmAttributeInfo { def name = "Signature" }
class InnerClassesAttr(val value: Array[JvmInnerClassInfo]) extends JvmAttributeInfo { def name = "InnerClasses" }

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
  class Builder(protected[this] val in: DataInput) extends ScalacClassfileModel {
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
    def readAttribute(): AttributeInfo = {
      val name = stringAt(u2)
      val len  = u4
      name match {
        case "Signature"    => new SignatureAttr(stringAt(u2))
        case "InnerClasses" => new InnerClassesAttr(readInnerClasses())
        case name           => val bytes = new Array[Byte](len) ; in.readFully(bytes) ; new GenericAttr(name, bytes)
      }
    }
  }

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
    new Builder(in) parse
  }
}
