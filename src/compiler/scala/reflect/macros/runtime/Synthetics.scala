/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 */

package scala.reflect.macros
package runtime

import scala.reflect.internal.Flags._
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.VirtualFile

trait Synthetics {
  self: Context =>

  import global._
  import analyzer._
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

  def introduceTopLevel[T: PackageSpec](packagePrototype: T, definition: universe.ImplDef): RefTree =
    introduceTopLevel(packagePrototype, List(definition)).head

  def introduceTopLevel[T: PackageSpec](packagePrototype: T, definitions: universe.ImplDef*): List[RefTree] =
    introduceTopLevel(packagePrototype, definitions.toList)

  private def introduceTopLevel[T: PackageSpec](packagePrototype: T, definitions: List[universe.ImplDef]): List[RefTree] = {
    val code @ PackageDef(pid, _) = implicitly[PackageSpec[T]].mkPackageDef(packagePrototype, definitions)
    universe.currentRun.compileLate(code)
    definitions map (definition => Select(pid, definition.name))
  }

  protected def mkPackageDef(name: String, stats: List[Tree]) = gen.mkPackageDef(name, stats)

  protected def mkPackageDef(name: TermName, stats: List[Tree]) = gen.mkPackageDef(name.toString, stats)

  protected def mkPackageDef(tree: RefTree, stats: List[Tree]) = PackageDef(tree, stats)

  protected def mkPackageDef(sym: Symbol, stats: List[Tree]) = {
    assert(sym hasFlag PACKAGE, s"expected a package or package class symbol, found: $sym")
    gen.mkPackageDef(sym.fullName.toString, stats)
  }

  def introduceMember(owner: Symbol, member: DefTree): Symbol = introduceMembers(owner, List(member): _*).head

  // TODO: for now we support two cases
  // 1) the defining tree hasn't yet been typechecked
  // 2) the defining tree has been fully typechecked
  // 3) what about middle ground? what if introduceMember is called from a macro which expands in a body of a defining's member?
  // 4) what if the symbol hasn't been completed yet?
  // 5) what if it's a mixture of 1 and 2?
  // 6) what if owner has already been compiled
  def introduceMembers(owner: Symbol, members: DefTree*): List[Symbol] = { // TODO: test how this works for the result of a template type macro
    if (!currentRun.compiles(owner))
      throw new SynthesisException(s"can only introduce members to symbols being currently compiled. $owner is not such a symbol")
    if (phase.id > currentRun.typerPhase.id)
      throw new SynthesisException(s"can only introduce members in typer. current phase is $phase")

    val templ = templateOf(owner)
    val templ1 = deriveTemplate(templ)(_ ++ members)

    // step 1: enter members, so that their usages typecheck
    val ownerContext = contextOf(owner)
    val templContext = ownerContext.outer.make(templ1, owner, owner.info.decls) // TODO: can we trigger ImplDef completers without being haunted by cyclic ref errors?
    newNamer(templContext).enterSyms(templ1.body)

    // step 2: inject trees, so that their usages have backing bytecode
    rememberTemplate(owner, templ1)
    if (templ1.tpe != null) newTyper(templContext).typecheckTemplateMembers(owner)

    members.toList map (_.symbol)
  }
}
