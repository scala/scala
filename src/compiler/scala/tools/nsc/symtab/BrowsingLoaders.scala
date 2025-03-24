/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package symtab

import scala.tools.nsc.Reporting.WarningCategory
import scala.tools.nsc.io.AbstractFile

/** A subclass of SymbolLoaders that implements browsing behavior.
 *  This class should be used whenever file dependencies and recompile sets
 *  are managed automatically.
 */
abstract class BrowsingLoaders extends GlobalSymbolLoaders {
  val global: Global

  import global._
  import syntaxAnalyzer.{OutlineParser, MalformedInput}

  /** In browse mode, it can happen that an encountered symbol is already
   *  present. For instance, if the source file has a name different from
   *  the classes and objects it contains, the symbol loader will always
   *  reparse the source file. The symbols it encounters might already be loaded
   *  as class files. In this case we return the one which has a sourcefile
   *  (and the other has not), and issue an error if both have sourcefiles.
   */
  override protected def enterIfNew(owner: Symbol, member: Symbol, completer: SymbolLoader): Symbol = {
    completer.sourcefile match {
      case Some(src) =>
        (if (member.isModule) member.moduleClass else member).associatedFile = src
      case _ =>
    }
    val decls = owner.info.decls
    val existing = decls.lookup(member.name)
    if (existing == NoSymbol) {
      decls enter member
      member
    } else {
      val existingSourceFile = existing.sourceFile
      if (existingSourceFile == null) {
        decls unlink existing
        decls enter member
        member
      } else {
        val memberSourceFile = member.sourceFile
        if (memberSourceFile != null) {
          if (existingSourceFile != memberSourceFile)
            globalError(""+member+"is defined twice,"+
              "\n in "+existingSourceFile+
              "\n and also in "+memberSourceFile)
        }
        existing
      }
    }
  }

  /** Browse the top-level of given abstract file `src` and enter
   *  any encountered top-level classes and modules in `root`
   */
  def browseTopLevel(root: Symbol, src: AbstractFile): Unit = {

    class BrowserTraverser extends Traverser {
      var packagePrefix = ""
      var entered = 0
      def addPackagePrefix(pkg: Tree): Unit = pkg match {
        case Select(pre, name) =>
          addPackagePrefix(pre)
          packagePrefix += ("." + name)
        case Ident(name) =>
          if (name != nme.EMPTY_PACKAGE_NAME) { // mirrors logic in Namers, see createPackageSymbol
            if (packagePrefix.length != 0) packagePrefix += "."
            packagePrefix += name
          }
        case _ =>
          throw new MalformedInput(pkg.pos.point, "illegal tree node in package prefix: "+pkg)
      }

      private def inPackagePrefix(pkg: Tree)(op: => Unit): Unit = {
        val oldPrefix = packagePrefix
        addPackagePrefix(pkg)
        op
        packagePrefix = oldPrefix
      }

      override def traverse(tree: Tree): Unit = tree match {
        case PackageDef(pid, stats) =>
          inPackagePrefix(pid) { stats.foreach(traverse) }

        case ClassDef(_, name, _, _) =>
          if (packagePrefix == root.fullName) {
            enterClass(root, name.toString, new SourcefileLoader(src))
            entered += 1
          } else log("prefixes differ: "+packagePrefix+","+root.fullName)
        case ModuleDef(_, name, _) =>
          if (packagePrefix == root.fullName) {
            val module = enterModule(root, name.toString, new SourcefileLoader(src))
            entered += 1
            if (name == nme.PACKAGEkw) {
              log("open package module: "+module)
              openPackageModule(module, root)
            }
          } else log("prefixes differ: "+packagePrefix+","+root.fullName)
        case _ =>
      }
    }

//    System.out.println("Browsing "+src)
    val source = getSourceFile(src) // this uses the current encoding
    val body = new OutlineParser(source).parse()
//    System.out.println(body)
    val browser = new BrowserTraverser
    browser.traverse(body)
    if (browser.entered == 0)
      runReporting.warning(NoPosition, "No classes or objects found in "+source+" that go in "+root, WarningCategory.OtherDebug, site = "")
  }

  /** Enter top-level symbols from a source file
   */
  override def enterToplevelsFromSource(root: Symbol, name: TermName, src: AbstractFile): Unit = {
    try {
      if (root.isEffectiveRoot || !src.name.endsWith(".scala")) // RootClass or EmptyPackageClass
        super.enterToplevelsFromSource(root, name, src)
      else
        browseTopLevel(root, src)
    } catch {
      case ex: syntaxAnalyzer.MalformedInput =>
        log(s"[$src] caught malformed input exception at offset ${ex.offset}: ${ex.msg}")
        super.enterToplevelsFromSource(root, name, src)
    }
  }
}
