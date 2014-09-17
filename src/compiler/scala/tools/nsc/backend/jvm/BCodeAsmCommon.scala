/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc.backend.jvm

import scala.tools.nsc.Global
import PartialFunction._

/**
 * This trait contains code shared between GenBCode and GenASM that depends on types defined in
 * the compiler cake (Global).
 */
final class BCodeAsmCommon[G <: Global](val global: G) {
  import global._

  /**
   * True if `classSym` is an anonymous class or a local class. I.e., false if `classSym` is a
   * member class. This method is used to decide if we should emit an EnclosingMethod attribute.
   * It is also used to decide whether the "owner" field in the InnerClass attribute should be
   * null.
   */
  def isAnonymousOrLocalClass(classSym: Symbol): Boolean = {
    assert(classSym.isClass, s"not a class: $classSym")
    val res = (classSym.isAnonymousClass || !classSym.originalOwner.isClass)
    // lambda classes are always top-level classes.
    if (res) assert(!classSym.isDelambdafyFunction)
    res
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

  /**
   * This is basically a re-implementation of sym.isStaticOwner, but using the originalOwner chain.
   *
   * The problem is that we are interested in a source-level property. Various phases changed the
   * symbol's properties in the meantime, mostly lambdalift modified (destructively) the owner.
   * Therefore, `sym.isStatic` is not what we want. For example, in
   *   object T { def f { object U } }
   * the owner of U is T, so UModuleClass.isStatic is true. Phase travel does not help here.
   */
  def isOriginallyStaticOwner(sym: Symbol): Boolean = {
    sym.isPackageClass || sym.isModuleClass && isOriginallyStaticOwner(sym.originalOwner)
  }

  /**
   * The member classes of a class symbol. Note that the result of this method depends on the
   * current phase, for example, after lambdalift, all local classes become member of the enclosing
   * class.
   */
  def memberClassesOf(classSymbol: Symbol): List[Symbol] = classSymbol.info.decls.collect({
    case sym if sym.isClass =>
      sym
    case sym if sym.isModule =>
      val r = exitingPickler(sym.moduleClass)
      assert(r != NoSymbol, sym.fullLocationString)
      r
  })(collection.breakOut)
}
