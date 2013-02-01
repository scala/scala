/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 */

package scala.reflect.macros
package runtime

import java.util.UUID._
import scala.reflect.internal.Flags._
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.VirtualFile

trait Synthetics {
  self: Context =>

  import global._
  import mirror.wrapMissing

  // getClassIfDefined and getModuleIfDefined cannot be used here
  // because they don't work for stuff declared in the empty package
  // (as specified in SLS, code inside non-empty packages cannot see
  // declarations from the empty package, so compiler internals
  // default to ignoring contents of the empty package)
  // to the contrast, staticModule and staticClass are designed
  // to be a part of the reflection API and, therefore, they
  // correctly resolve all names
  private def topLevelSymbol(name: Name): Symbol = wrapMissing {
    if (name.isTermName) mirror.staticModule(name.toString)
    else mirror.staticClass(name.toString)
  }

  def topLevelDef(name: Name): Tree =
    enclosingRun.units.toList.map(_.body).flatMap {
      // it's okay to check `stat.symbol` here, because currently macros expand strictly after namer
      // which means that by the earliest time one can call this method all top-level definitions will have already been entered
      case PackageDef(_, stats) => stats filter (stat => stat.symbol != NoSymbol && stat.symbol == topLevelSymbol(name))
      case _ => Nil // should never happen, but better be safe than sorry
    }.headOption getOrElse EmptyTree

  def topLevelRef(name: Name): Tree = {
    if (topLevelDef(name).nonEmpty) gen.mkUnattributedRef(name)
    else EmptyTree
  }

  // TODO: provide a way to specify a pretty name for debugging purposes
  private def randomFileName() = (
    "macroSynthetic-" + randomUUID().toString.replace("-", "") + ".scala"
  )

  def introduceTopLevel[T: PackageSpec](packagePrototype: T, definition: universe.ImplDef): RefTree =
    introduceTopLevel(packagePrototype, List(definition)).head

  def introduceTopLevel[T: PackageSpec](packagePrototype: T, definitions: universe.ImplDef*): List[RefTree] =
    introduceTopLevel(packagePrototype, definitions.toList)

  private def introduceTopLevel[T: PackageSpec](packagePrototype: T, definitions: List[universe.ImplDef]): List[RefTree] = {
    val code @ PackageDef(pid, _) = implicitly[PackageSpec[T]].mkPackageDef(packagePrototype, definitions)
    val syntheticFileName = randomFileName()
    // compatibility with SBT
    // on the one hand, we need to specify some jfile here, otherwise sbt crashes with an NPE (SI-6870)
    // on the other hand, we can't specify the obvious enclosingUnit, because then sbt somehow fails to run tests using type macros
    // okay, now let's specify a guaranteedly non-existent file in an existing directory (so that we don't run into permission problems)
    val relatedJfile = enclosingUnit.source.file.file
    val fakeJfile = if (relatedJfile != null) new java.io.File(relatedJfile.getParent, syntheticFileName) else null
    val virtualFile = new VirtualFile(syntheticFileName) { override def file = fakeJfile }
    val sourceFile = new BatchSourceFile(virtualFile, code.toString)
    val unit = new CompilationUnit(sourceFile)
    unit.body = code
    universe.currentRun.compileLate(unit)
    definitions map (definition => Select(pid, definition.name))
  }

  protected def mkPackageDef(name: String, stats: List[Tree]) = gen.mkPackageDef(name, stats)

  protected def mkPackageDef(name: TermName, stats: List[Tree]) = gen.mkPackageDef(name.toString, stats)

  protected def mkPackageDef(tree: RefTree, stats: List[Tree]) = PackageDef(tree, stats)

  protected def mkPackageDef(sym: Symbol, stats: List[Tree]) = {
    assert(sym hasFlag PACKAGE, s"expected a package or package class symbol, found: $sym")
    gen.mkPackageDef(sym.fullName.toString, stats)
  }
}
