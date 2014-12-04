/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc.backend.jvm

import scala.reflect.internal.AnnotationInfos
import scala.tools.nsc.Global

/**
 * This trait contains code shared between GenBCode and GenASM that depends on types defined in
 * the compiler cake (Global).
 */
final class BCodeAsmCommon[I <: BackendInterface](interface: I) {
import interface._

  /**
   * True if `classSym` is an anonymous class or a local class. I.e., false if `classSym` is a
   * member class. This method is used to decide if we should emit an EnclosingMethod attribute.
   * It is also used to decide whether the "owner" field in the InnerClass attribute should be
   * null.
   */
  def isAnonymousOrLocalClass(classSym: Symbol): Boolean = {
    assert(classSym.isClass, s"not a class: $classSym")
    // Here used to be an `assert(!classSym.isDelambdafyFunction)`: delambdafy lambda classes are
    // always top-level. However, SI-8900 shows an example where the weak name-based implementation
    // of isDelambdafyFunction failed (for a function declared in a package named "lambda").
    classSym.isAnonymousClass || (classSym.originalOwner != NoSymbol && !classSym.originalOwner.isClass)
  }

  /**
   * Returns the enclosing method for non-member classes. In the following example
   *
   * class A {
   *   def f = {
   *     class B {
   *       class C
   *     }
   *   }
   * }
   *
   * the method returns Some(f) for B, but None for C, because C is a member class. For non-member
   * classes that are not enclosed by a method, it returns None:
   *
   * class A {
   *   { class B }
   * }
   *
   * In this case, for B, we return None.
   *
   * The EnclosingMethod attribute needs to be added to non-member classes (see doc in BTypes).
   * This is a source-level property, so we need to use the originalOwner chain to reconstruct it.
   */
  private def enclosingMethodForEnclosingMethodAttribute(classSym: Symbol): Option[Symbol] = {
    assert(classSym.isClass, classSym)
    def enclosingMethod(sym: Symbol): Option[Symbol] = {
      if (sym.isClass || sym == NoSymbol) None
      else if (sym.isMethod) Some(sym)
      else enclosingMethod(sym.originalOwner)
    }
    enclosingMethod(classSym.originalOwner)
  }

  /**
   * The enclosing class for emitting the EnclosingMethod attribute. Since this is a source-level
   * property, this method looks at the originalOwner chain. See doc in BTypes.
   */
  private def enclosingClassForEnclosingMethodAttribute(classSym: Symbol): Symbol = {
    assert(classSym.isClass, classSym)
    def enclosingClass(sym: Symbol): Symbol = {
      if (sym.isClass) sym
      else enclosingClass(sym.originalOwner)
    }
    enclosingClass(classSym.originalOwner)
  }

  final case class EnclosingMethodEntry(owner: String, name: String, methodDescriptor: String)

  /**
   * Data for emitting an EnclosingMethod attribute. None if `classSym` is a member class (not
   * an anonymous or local class). See doc in BTypes.
   *
   * The class is parametrized by two functions to obtain a bytecode class descriptor for a class
   * symbol, and to obtain a method signature descriptor fro a method symbol. These function depend
   * on the implementation of GenASM / GenBCode, so they need to be passed in.
   */
  def enclosingMethodAttribute(classSym: Symbol, classDesc: Symbol => String, methodDesc: Symbol => String): Option[EnclosingMethodEntry] = {
    if (isAnonymousOrLocalClass(classSym)) {
      val methodOpt = enclosingMethodForEnclosingMethodAttribute(classSym)
      debuglog(s"enclosing method for $classSym is $methodOpt (in ${methodOpt.map(_.enclClass)})")
      Some(EnclosingMethodEntry(
        classDesc(enclosingClassForEnclosingMethodAttribute(classSym)),
        methodOpt.map(_.javaSimpleName.toString).orNull,
        methodOpt.map(methodDesc).orNull))
    } else {
      None
    }
  }
}

object BCodeAsmCommon{
  def ubytesToCharArray(bytes: Array[Byte]): Array[Char] = {
    val ca = new Array[Char](bytes.length)
    var idx = 0
    while(idx < bytes.length) {
      val b: Byte = bytes(idx)
      assert((b & ~0x7f) == 0)
      ca(idx) = b.asInstanceOf[Char]
      idx += 1
    }

    ca
  }

  final def arrEncode(sb: AnnotationInfos#ScalaSigBytes): Array[String] = {
    var strs: List[String]  = Nil
    val bSeven: Array[Byte] = sb.sevenBitsMayBeZero
    // chop into slices of at most 65535 bytes, counting 0x00 as taking two bytes (as per JVMS 4.4.7 The CONSTANT_Utf8_info Structure)
    var prevOffset = 0
    var offset     = 0
    var encLength  = 0
    while(offset < bSeven.length) {
      val deltaEncLength = (if(bSeven(offset) == 0) 2 else 1)
      val newEncLength = encLength.toLong + deltaEncLength
      if(newEncLength >= 65535) {
        val ba     = bSeven.slice(prevOffset, offset)
        strs     ::= new java.lang.String(ubytesToCharArray(ba))
        encLength  = 0
        prevOffset = offset
      } else {
        encLength += deltaEncLength
        offset    += 1
      }
    }
    if(prevOffset < offset) {
      assert(offset == bSeven.length)
      val ba = bSeven.slice(prevOffset, offset)
      strs ::= new java.lang.String(ubytesToCharArray(ba))
    }
    assert(strs.size > 1, "encode instead as one String via strEncode()") // TODO too strict?
    strs.reverse.toArray
  }


  def strEncode(sb: AnnotationInfos#ScalaSigBytes): String = {
    val ca = ubytesToCharArray(sb.sevenBitsMayBeZero)
    new java.lang.String(ca)
    // debug val bvA = new asm.ByteVector; bvA.putUTF8(s)
    // debug val enc: Array[Byte] = scala.reflect.internal.pickling.ByteCodecs.encode(bytes)
    // debug assert(enc(idx) == bvA.getByte(idx + 2))
    // debug assert(bvA.getLength == enc.size + 2)
  }
}