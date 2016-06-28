/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.tools.asm._
import scala.tools.nsc.backend.jvm.BTypes.{InlineInfo, MethodInlineInfo}
import scala.tools.nsc.backend.jvm.BackendReporting.UnknownScalaInlineInfoVersion

/**
 * This attribute stores the InlineInfo for a ClassBType as an independent classfile attribute.
 * The compiler does so for every class being compiled.
 *
 * The reason is that a precise InlineInfo can only be obtained if the symbol for a class is available.
 * For example, we need to know if a method is final in Scala's terms, or if it has the @inline annotation.
 * Looking up a class symbol for a given class filename is brittle (name-mangling).
 *
 * The attribute is also helpful for inlining mixin methods. The mixin phase only adds mixin method
 * symbols to classes that are being compiled. For all other class symbols, there are no mixin members.
 * However, the inliner requires an InlineInfo for inlining mixin members. That problem is solved by
 * reading the InlineInfo from this attribute.
 *
 * In principle we could encode the InlineInfo into a Java annotation (instead of a classfile attribute).
 * However, an attribute allows us to save many bits. In particular, note that the strings in an
 * InlineInfo are serialized as references to constants in the constant pool, and those strings
 * (method names, method signatures) would exist in there anyway. So the
 * ScalaInlineAttribute remains relatively compact.
 */
case class InlineInfoAttribute(inlineInfo: InlineInfo) extends Attribute(InlineInfoAttribute.attributeName) {
  /**
   * Not sure what this method is good for, it is not invoked anywhere in the ASM framework. However,
   * the example in the ASM manual also overrides it to `false` for custom attributes, so it might be
   * a good idea.
   */
  override def isUnknown: Boolean = false

  /**
   * Serialize the `inlineInfo` into a byte array. Strings are added to the constant pool and serialized
   * as references.
   */
  override def write(cw: ClassWriter, code: Array[Byte], len: Int, maxStack: Int, maxLocals: Int): ByteVector = {
    val result = new ByteVector()

    result.putByte(InlineInfoAttribute.VERSION)

    var flags = 0
    if (inlineInfo.isEffectivelyFinal)      flags |= 1
    //                                      flags |= 2 // no longer written
    if (inlineInfo.sam.isDefined)           flags |= 4
    result.putByte(flags)

    for (samNameDesc <- inlineInfo.sam) {
      val (name, desc) = samNameDesc.span(_ != '(')
      result.putShort(cw.newUTF8(name))
      result.putShort(cw.newUTF8(desc))
    }

    // The method count fits in a short (the methods_count in a classfile is also a short)
    result.putShort(inlineInfo.methodInfos.size)

    // Sort the methodInfos for stability of classfiles
    for ((nameAndType, info) <- inlineInfo.methodInfos.toList.sortBy(_._1)) {
      val (name, desc) = nameAndType.span(_ != '(')
      // Name and desc are added separately because a NameAndType entry also stores them separately.
      // This makes sure that we use the existing constant pool entries for the method.
      result.putShort(cw.newUTF8(name))
      result.putShort(cw.newUTF8(desc))

      var inlineInfo = 0
      if (info.effectivelyFinal)  inlineInfo |= 1
      //                          inlineInfo |= 2 // no longer written
      if (info.annotatedInline)   inlineInfo |= 4
      if (info.annotatedNoInline) inlineInfo |= 8
      result.putByte(inlineInfo)
    }

    result
  }

  /**
   * De-serialize the attribute into an InlineInfo. The attribute starts at cr.b(off), but we don't
   * need to access that array directly, we can use the `read` methods provided by the ClassReader.
   *
   * `buf` is a pre-allocated character array that is guaranteed to be long enough to hold any
   * string of the constant pool. So we can use it to invoke `cr.readUTF8`.
   */
  override def read(cr: ClassReader, off: Int, len: Int, buf: Array[Char], codeOff: Int, labels: Array[Label]): InlineInfoAttribute = {
    var next = off

    def nextByte()  = { val r = cr.readByte(next)     ; next += 1; r }
    def nextUTF8()  = { val r = cr.readUTF8(next, buf); next += 2; r }
    def nextShort() = { val r = cr.readShort(next)    ; next += 2; r }

    val version = nextByte()
    if (version == 1) {
      val flags = nextByte()
      val isFinal           = (flags & 1) != 0
      val hasSelf           = (flags & 2) != 0
      val hasSam            = (flags & 4) != 0

      if (hasSelf) nextUTF8() // no longer used

      val sam = if (!hasSam) None else {
        val name = nextUTF8()
        val desc = nextUTF8()
        Some(name + desc)
      }

      val numEntries = nextShort()
      val infos = (0 until numEntries).map(_ => {
        val name = nextUTF8()
        val desc = nextUTF8()

        val inlineInfo = nextByte()
        val isFinal    = (inlineInfo & 1) != 0
        //             = (inlineInfo & 2) != 0 // no longer used
        val isInline   = (inlineInfo & 4) != 0
        val isNoInline = (inlineInfo & 8) != 0
        (name + desc, MethodInlineInfo(isFinal, isInline, isNoInline))
      }).toMap

      val info = InlineInfo(isFinal, sam, infos, None)
      InlineInfoAttribute(info)
    } else {
      val msg = UnknownScalaInlineInfoVersion(cr.getClassName, version)
      InlineInfoAttribute(BTypes.EmptyInlineInfo.copy(warning = Some(msg)))
    }
  }
}

object InlineInfoAttribute {
  /**
   * Notes:
   *  - `traitImplClassSelfType` is no longer emitted, `hasTraitImplClassSelfType` is always emitted
   *    as 0. Similarly, `traitMethodWithStaticImplementation` is always emitted 0.
   *  - When reading an existing attribute where `hasTraitImplClassSelfType` is 1, the
   *    `traitImplClassSelfType` is ignored. Also the value of `traitMethodWithStaticImplementation`
   *    is ignored.
   *
   * [u1]    version
   * [u1]    isEffectivelyFinal (<< 0), hasTraitImplClassSelfType (<< 1), hasSam (<< 2), hasLateInterfaces (<< 3)
   * [u2]?   traitImplClassSelfType (reference)
   * [u2]?   samName (reference)
   * [u2]?   samDescriptor (reference)
   * [u2]    numMethodEntries
   *   [u2]  name (reference)
   *   [u2]  descriptor (reference)
   *   [u1]  isFinal (<< 0), traitMethodWithStaticImplementation (<< 1), hasInlineAnnotation (<< 2), hasNoInlineAnnotation (<< 3)
   */
  final val VERSION: Byte = 1

  final val attributeName = "ScalaInlineInfo"
}

/**
 * In order to instruct the ASM framework to de-serialize the ScalaInlineInfo attribute, we need
 * to pass a prototype instance when running the class reader.
 */
object InlineInfoAttributePrototype extends InlineInfoAttribute(InlineInfo(false, null, null, null))
