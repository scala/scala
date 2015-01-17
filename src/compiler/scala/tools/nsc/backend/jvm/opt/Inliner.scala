/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.tools.asm
import asm.Opcodes
import asm.tree._
import scala.collection.convert.decorateAsScala._
import OptimizerReporting._

class Inliner[BT <: BTypes](val btypes: BT) {
  import btypes._
  import btypes.byteCodeRepository

  def findIllegalAccess(instructions: InsnList, destinationClass: ClassBType): Option[AbstractInsnNode] = {

    /**
     * Check if a type is accessible to some class, as defined in JVMS 5.4.4.
     *  (A1) C is public
     *  (A2) C and D are members of the same run-time package
     */
    def classIsAccessible(accessed: BType, from: ClassBType = destinationClass): Boolean = (accessed: @unchecked) match {
      // TODO: A2 requires "same run-time package", which seems to be package + classloader (JMVS 5.3.). is the below ok?
      case c: ClassBType     => c.isPublic || c.packageInternalName == from.packageInternalName
      case a: ArrayBType     => classIsAccessible(a.elementType, from)
      case _: PrimitiveBType => true
    }

    /**
     * Check if a member reference is accessible from the [[destinationClass]], as defined in the
     * JVMS 5.4.4. Note that the class name in a field / method reference is not necessarily the
     * class in which the member is declared:
     *
     *   class A { def f = 0 }; class B extends A { f }
     *
     * The INVOKEVIRTUAL instruction uses a method reference "B.f ()I". Therefore this method has
     * two parameters:
     *
     * @param memberDeclClass The class in which the member is declared (A)
     * @param memberRefClass  The class used in the member reference (B)
     *
     * JVMS 5.4.4 summary: A field or method R is accessible to a class D (destinationClass) iff
     *  (B1) R is public
     *  (B2) R is protected, declared in C (memberDeclClass) and D is a subclass of C.
     *       If R is not static, R must contain a symbolic reference to a class T (memberRefClass),
     *       such that T is either a subclass of D, a superclass of D, or D itself.
     *  (B3) R is either protected or has default access and declared by a class in the same
     *       run-time package as D.
     *  (B4) R is private and is declared in D.
     */
    def memberIsAccessible(memberFlags: Int, memberDeclClass: ClassBType, memberRefClass: ClassBType): Boolean = {
      // TODO: B3 requires "same run-time package", which seems to be package + classloader (JMVS 5.3.). is the below ok?
      def samePackageAsDestination = memberDeclClass.packageInternalName == destinationClass.packageInternalName

      val key = (Opcodes.ACC_PUBLIC | Opcodes.ACC_PROTECTED | Opcodes.ACC_PRIVATE) & memberFlags
      key match {
        case Opcodes.ACC_PUBLIC     =>       // B1
          true

        case Opcodes.ACC_PROTECTED  =>       // B2
          val condB2 = destinationClass.isSubtypeOf(memberDeclClass) && {
            val isStatic = (Opcodes.ACC_STATIC & memberFlags) != 0
            isStatic || memberRefClass.isSubtypeOf(destinationClass) || destinationClass.isSubtypeOf(memberRefClass)
          }
          condB2 || samePackageAsDestination // B3 (protected)

        case 0 =>                            // B3 (default access)
          samePackageAsDestination

        case Opcodes.ACC_PRIVATE    =>       // B4
          memberDeclClass == destinationClass
      }
    }

    def isLegal(instruction: AbstractInsnNode): Boolean = instruction match {
      case ti: TypeInsnNode  =>
        // NEW, ANEWARRAY, CHECKCAST or INSTANCEOF. For these instructions, the reference
        // "must be a symbolic reference to a class, array, or interface type" (JVMS 6), so
        // it can be an internal name, or a full array descriptor.
        classIsAccessible(bTypeForDescriptorOrInternalNameFromClassfile(ti.desc))

      case ma: MultiANewArrayInsnNode =>
        // "a symbolic reference to a class, array, or interface type"
        classIsAccessible(bTypeForDescriptorOrInternalNameFromClassfile(ma.desc))

      case fi: FieldInsnNode =>
        val fieldRefClass = classBTypeFromParsedClassfile(fi.owner)
        val (fieldNode, fieldDeclClass) = byteCodeRepository.fieldNode(fieldRefClass.internalName, fi.name, fi.desc).get
        memberIsAccessible(fieldNode.access, classBTypeFromParsedClassfile(fieldDeclClass), fieldRefClass)

      case mi: MethodInsnNode =>
        if (mi.owner.charAt(0) == '[') true // array methods are accessible
        else {
          val methodRefClass = classBTypeFromParsedClassfile(mi.owner)
          val (methodNode, methodDeclClass) = byteCodeRepository.methodNode(methodRefClass.internalName, mi.name, mi.desc).get
          memberIsAccessible(methodNode.access, classBTypeFromParsedClassfile(methodDeclClass), methodRefClass)
        }

      case ivd: InvokeDynamicInsnNode =>
        // TODO @lry check necessary conditions to inline an indy, instead of giving up
        false

      case ci: LdcInsnNode => ci.cst match {
        case t: asm.Type => classIsAccessible(bTypeForDescriptorOrInternalNameFromClassfile(t.getInternalName))
        case _           => true
      }

      case _ => true
    }

    instructions.iterator.asScala.find(!isLegal(_))
  }
}
