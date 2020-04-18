package scala.tools.nsc.symtab

import org.junit.Test

import scala.reflect.io.NoAbstractFile
import scala.tools.nsc.{Global, Settings}

class SymbolLoadersAssociatedFileTest {
  @Test
  def classSymbolAssociatedFileIsSetWhenModuleIsReferencedFirst(): Unit = {
    val g = new Global(new Settings)
    g.settings.usejavacp.value = true
    g.settings.stopAfter.value = List("typer")
    val r = new g.Run()
    r.compileUnits(g.newCompilationUnit("class Test { identity(Tuple5) }") :: Nil, r.parserPhase)
    val Tuple5_class = g.rootMirror.getRequiredClass("scala.Tuple5")
    Tuple5_class.info
    assert(Tuple5_class.associatedFile != NoAbstractFile)
    assert(Tuple5_class.associatedFile.name == "Tuple5.class", Tuple5_class.associatedFile.name)
  }

  @Test
  def moduleSymbolAssociatedFileIsSetWhenClassIsReferencedFirst(): Unit = {
    val g = new Global(new Settings)
    g.settings.usejavacp.value = true
    g.settings.stopAfter.value = List("typer")
    val r2 = new g.Run()
    r2.compileUnits(g.newCompilationUnit("class Test { identity[Tuple5[Any, Any, Any, Any, Any]](null) }") :: Nil, r2.parserPhase)
    val Tuple5_module = g.rootMirror.getRequiredModule("scala.Tuple5")
    Tuple5_module.info
    assert(Tuple5_module.associatedFile != NoAbstractFile)
    assert(Tuple5_module.associatedFile.name == "Tuple5.class", Tuple5_module.associatedFile.name)
  }
}
