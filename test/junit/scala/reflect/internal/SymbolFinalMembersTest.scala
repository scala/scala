package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test

import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.symtab.SymbolTableForUnitTesting

class SymbolFinalMembersTest {
  object symbolTable extends SymbolTableForUnitTesting {
    override val reporter = new StoreReporter(settings)
  }
  import symbolTable._

  // Reads false, and silently turns every cache into an allocation, if the flag is set after the mixins' initializers.
  @Test def reusableInstancesAreEnabledInACompilerUniverse(): Unit = {
    assertTrue(isCompilerUniverse)
    assertSame(findMemberInstance.using(x => x), findMemberInstance.using(x => x))
  }

  // ReflectGlobal is a compiler universe, but its symbols must keep runtime reflection's locks.
  @Test def runtimeReflectionSymbolsLockAlsoInReflectGlobal(): Unit = {
    assertFalse(hasSynchronizedSymbols)
    assertTrue(scala.reflect.runtime.universe.asInstanceOf[scala.reflect.runtime.JavaUniverse].hasSynchronizedSymbols)
    val rg = new scala.tools.reflect.ReflectGlobal(settings, new StoreReporter(settings), getClass.getClassLoader)
    assertTrue(rg.isCompilerUniverse)
    assertTrue(rg.hasSynchronizedSymbols)
  }

  @Test def kindPredicates(): Unit = {
    definitions.init()
    import definitions._
    val root = rootMirror.RootClass
    for (pkg <- List[Symbol](root, rootMirror.EmptyPackageClass, ScalaPackageClass)) {
      assertTrue(pkg.toString, pkg.isClass)
      assertTrue(pkg.toString, pkg.isPackageClass)
    }
    for (sym <- List[Symbol](ListClass, ListClass.companionModule.moduleClass, NoSymbol, ScalaPackage, ListClass.typeParams.head)) {
      assertFalse(sym.toString, sym.isPackageClass)
      assertFalse(sym.toString, sym.isRefinementClass)
    }
    val refinement = root.newRefinementClass(NoPosition)
    assertTrue(refinement.isClass)
    assertTrue(refinement.isRefinementClass)
    assertFalse(ListClass.typeParams.head.isClass)
    assertFalse(NoSymbol.isClass)
  }

  @Test def noSymbolIsItsOwnOwnerAndItsInfoIsNoType(): Unit = {
    assertSame(NoSymbol, NoSymbol.owner)
    assertSame(NoType, NoSymbol.info)
    assertSame(NoType, NoSymbol.rawInfo)
  }

  @Test def stubReportsMissingSymbolEvenAfterSetInfo(): Unit = {
    val stub = rootMirror.EmptyPackageClass.newStubSymbol(TypeName("Missing"), "missing")
    stub.setInfo(NoType)
    assertFalse(stub.isInitialized)
    assertFalse(reporter.hasErrors)
    assertSame(NoType, stub.info)
    assertTrue(reporter.hasErrors)
  }

  @Test def stubReportsMissingSymbolEvenAfterUpdateInfo(): Unit = {
    val stub = rootMirror.EmptyPackageClass.newStubSymbol(TypeName("Missing"), "missing")
    stub.setInfo(NoType)
    stub.updateInfo(NoType)
    assertFalse(stub.isInitialized)
    assertFalse(reporter.hasErrors)
    assertSame(NoType, stub.info)
    assertTrue(reporter.hasErrors)
  }

  @Test def ownerAfterFlatten(): Unit = {
    val pkg = rootMirror.RootClass.newPackage(TermName("p")).moduleClass
    val outer = pkg.newClassSymbol(TypeName("Outer"))
    val cls = outer.newClassSymbol(TypeName("Inner"))
    val mod = outer.newModule(TermName("M"))
    val modMethod = outer.newModule(TermName("N"), NoPosition, Flags.METHOD)
    val term = outer.newValue(TermName("v"))

    for (sym <- List[Symbol](cls, mod, modMethod, term)) assertSame(sym.name.toString, outer, sym.owner)
    // not SomePhase: a Phase's constructor links itself into its predecessor
    val flatten = new Phase(NoPhase) { def name = "flatten"; def run() = () }
    val after = new Phase(flatten) { def name = "after"; def run() = () }
    assertTrue(after.flatClasses)
    val saved = phase
    phase = after
    try {
      assertSame(pkg, cls.owner)
      assertSame(pkg, mod.owner)
      assertSame(outer, modMethod.owner)
      assertSame(outer, term.owner)
      assertSame(rootMirror.RootClass, outer.owner.owner)
    } finally phase = saved
  }
}
