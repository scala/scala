package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm.Opcodes._
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.BackendReporting._
import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class BTypesFromClassfileTest extends BytecodeTesting {
  // inliner enabled -> inlineInfos are collected (and compared) in ClassBTypes
  override def compilerArgs = "-opt:inline-global"

  import compiler.global._
  import definitions._
  import genBCode.bTypes
  import bTypes._

  def duringBackend[T](f: => T) = global.exitingDelambdafy(f)

  val run = new global.Run() // initializes some of the compiler
  duringBackend(global.scalaPrimitives.init()) // needed: it's only done when running the backend, and we don't actually run the compiler
  duringBackend(bTypes.initializeCoreBTypes())

  def clearCache() = bTypes.classBTypeFromInternalName.clear()

  def sameBType(fromSym: ClassBType, fromClassfile: ClassBType, checked: Set[InternalName] = Set.empty): Set[InternalName] = {
    if (checked(fromSym.internalName)) checked
    else {
      assert(fromSym == fromClassfile, s"$fromSym != $fromClassfile")
      sameInfo(fromSym.info.get, fromClassfile.info.get, checked + fromSym.internalName)
    }
  }

  def sameBTypes(fromSyms: Iterable[ClassBType], fromClassfiles: Iterable[ClassBType], checked: Set[InternalName]): Set[InternalName] = {
    assert(fromSyms.size == fromClassfiles.size, s"\n$fromSyms\n$fromClassfiles")
    (fromSyms, fromClassfiles).zipped.foldLeft(checked) {
      case (chk, (fromSym, fromClassfile)) => sameBType(fromSym, fromClassfile, chk)
    }
  }

  def sameInfo(fromSym: ClassInfo, fromClassfile: ClassInfo, checked: Set[InternalName]): Set[InternalName] = {
    assert({
      // Nested class symbols can undergo makeNotPrivate (ExplicitOuter). But this is only applied
      // for symbols of class symbols that are being compiled, not those read from a pickle.
      // So a class may be public in bytecode, but the symbol still says private.
      if (fromSym.nestedInfo.isEmpty) fromSym.flags == fromClassfile.flags
      else (fromSym.flags | ACC_PRIVATE | ACC_PUBLIC) == (fromClassfile.flags | ACC_PRIVATE | ACC_PUBLIC)
    }, s"class flags differ\n$fromSym\n$fromClassfile")

    // we don't compare InlineInfos in this test: in both cases (from symbol and from classfile) they
    // are actually created by looking at the classfile members, not the symbol's. InlineInfos are only
    // built from symbols for classes that are being compiled, which is not the case here. Instead
    // there's a separate InlineInfoTest.

    val chk1 = sameBTypes(fromSym.superClass, fromClassfile.superClass, checked)
    val chk2 = sameBTypes(fromSym.interfaces, fromClassfile.interfaces, chk1)

    // The fromSym info has only member classes, no local or anonymous. The symbol is read from the
    // Scala pickle data and only member classes are created / entered.
    //  (This is different for symbols that are being compiled, there flatten will enter all local
    //   and anonymous classes as members of the outer class. But not for unpickled symbols).
    // The fromClassfile info has all nested classes, including anonymous and local. So we filter
    // them out: member classes are identified by having the `outerName` defined.
    val memberClassesFromClassfile = fromClassfile.nestedClasses.filter(_.info.get.nestedInfo.get.outerName.isDefined)
    // Sorting is required: the backend sorts all InnerClass entries by internalName before writing
    // them to the classfile (to make it deterministic: the entries are collected in a Set during
    // code generation).
    val chk3 = sameBTypes(fromSym.nestedClasses.sortBy(_.internalName), memberClassesFromClassfile.sortBy(_.internalName), chk2)
    sameBTypes(fromSym.nestedInfo.map(_.enclosingClass), fromClassfile.nestedInfo.map(_.enclosingClass), chk3)
  }

  def check(classSym: Symbol): Unit = duringBackend {
    clearCache()
    val fromSymbol = classBTypeFromSymbol(classSym)
    clearCache()
    val fromClassfile = bTypes.classBTypeFromParsedClassfile(fromSymbol.internalName)
    sameBType(fromSymbol, fromClassfile)
  }

  @Test
  def compareClassBTypes(): Unit = {
    // Note that not only these classes are tested, but also all their parents and all nested
    // classes in their InnerClass attributes.
    check(ObjectClass)
    check(JavaNumberClass)
    check(ConsClass)
    check(ListModule.moduleClass)
  }
}
