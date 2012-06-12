package scala.reflect.reify
package utils

import scala.collection._
import scala.compat.Platform.EOL

trait SymbolTables {
  self: Utils =>

  import global._
  import definitions._
  import Flag._

  class SymbolTable private[SymbolTable] (
    private[SymbolTable] val symtab: immutable.ListMap[Symbol, Tree] = immutable.ListMap[Symbol, Tree](),
    private[SymbolTable] val aliases: List[(Symbol, TermName)] = List[(Symbol, TermName)](),
    private[SymbolTable] val original: Option[List[Tree]] = None) {

    def syms: List[Symbol] = symtab.keys.toList

//    def aliases: Map[Symbol, List[TermName]] = aliases.distinct groupBy (_._1) mapValues (_ map (_._2))

    def symDef(sym: Symbol): Tree =
      symtab.getOrElse(sym, EmptyTree)

    def symName(sym: Symbol): TermName =
      symtab.get(sym) match {
        case Some(FreeDef(_, name, _, _, _)) => name
        case Some(SymDef(_, name, _, _)) => name
        case None => EmptyTermName
      }

    def symAliases(sym: Symbol): List[TermName] =
      symName(sym) match {
        case name if name.isEmpty => Nil
        case _ => (aliases.distinct groupBy (_._1) mapValues (_ map (_._2)))(sym)
      }

    def symBinding(sym: Symbol): Tree =
      symtab.get(sym) match {
        case Some(FreeDef(_, _, binding, _, _)) => binding
        case Some(SymDef(_, _, _, _)) => throw new UnsupportedOperationException(s"${symtab(sym)} is a symdef, hence it doesn't have a binding")
        case None => EmptyTree
      }

    def symRef(sym: Symbol): Tree =
      symtab.get(sym) match {
        case Some(FreeDef(_, name, _, _, _)) => Ident(name) addAttachment ReifyBindingAttachment(sym)
        case Some(SymDef(_, name, _, _)) => Ident(name) addAttachment ReifyBindingAttachment(sym)
        case None => EmptyTree
      }

    def +(sym: Symbol, name: TermName, reification: Tree): SymbolTable = add(sym, name, reification)
    def +(sym: Symbol, name: TermName): SymbolTable = add(sym, name)
    def +(symDef: Tree): SymbolTable = add(symDef)
    def ++(symDefs: TraversableOnce[Tree]): SymbolTable = (this /: symDefs)((symtab, symDef) => symtab.add(symDef))
    def ++(symtab: SymbolTable): SymbolTable = { val updated = this ++ symtab.symtab.values; new SymbolTable(updated.symtab, updated.aliases ++ symtab.aliases) }
    def -(sym: Symbol): SymbolTable = remove(sym)
    def -(name: TermName): SymbolTable = remove(name)
    def -(symDef: Tree): SymbolTable = remove(binding(symDef))
    def --(syms: GenTraversableOnce[Symbol]): SymbolTable = (this /: syms)((symtab, sym) => symtab.remove(sym))
    def --(names: Iterable[TermName]): SymbolTable = (this /: names)((symtab, name) => symtab.remove(name))
    def --(symDefs: TraversableOnce[Tree]): SymbolTable = this -- (symDefs map (binding(_)))
    def --(symtab: SymbolTable): SymbolTable = { val updated = this -- symtab.symtab.values; new SymbolTable(updated.symtab, updated.aliases diff symtab.aliases) }
    def filterSyms(p: Symbol => Boolean): SymbolTable = this -- (syms filterNot p)
    def filterAliases(p: (Symbol, TermName) => Boolean): SymbolTable = this -- (aliases filterNot (tuple => p(tuple._1, tuple._2)) map (_._2))

    private def add(symDef: Tree): SymbolTable = {
      val sym = binding(symDef)
      assert(sym != NoSymbol, showRaw(symDef))
      val name = symDef match {
        case FreeDef(_, name, _, _, _) => name
        case SymDef(_, name, _, _) => name
      }
      val newSymtab = if (!(symtab contains sym)) symtab + (sym -> symDef) else symtab
      val newAliases = aliases :+ (sym -> name)
      new SymbolTable(newSymtab, newAliases)
    }

    private def add(sym: Symbol, name0: TermName, reification: Tree): SymbolTable = {
      def freshName(name0: TermName): TermName = {
        var name = name0.toString
        name = name.replace(".type", "$type")
        name = name.replace(" ", "$")
        val fresh = typer.context.unit.fresh
        newTermName(fresh.newName(name))
      }
      add(ValDef(NoMods, freshName(name0), TypeTree(), reification) addAttachment ReifyBindingAttachment(sym))
    }

    private def add(sym: Symbol, name: TermName): SymbolTable = {
      if (!(syms contains sym)) error("cannot add an alias to a symbol not in the symbol table")
      add(sym, name, EmptyTree)
    }

    private def remove(sym: Symbol): SymbolTable = {
      val newSymtab = symtab - sym
      val newAliases = aliases filter (_._1 != sym)
      new SymbolTable(newSymtab, newAliases)
    }

    private def remove(name: TermName): SymbolTable = {
      var newSymtab = symtab
      val newAliases = aliases filter (_._2 != name)
      newSymtab = newSymtab filter { case ((sym, _)) => newAliases exists (_._1 == sym) }
      newSymtab = newSymtab map { case ((sym, tree)) =>
        val ValDef(mods, primaryName, tpt, rhs) = tree
        val tree1 =
          if (!(newAliases contains (sym, primaryName))) {
            val primaryName1 = newAliases.find(_._1 == sym).get._2
            ValDef(mods, primaryName1, tpt, rhs).copyAttrs(tree)
          } else tree
        (sym, tree1)
      }
      new SymbolTable(newSymtab, newAliases)
    }

    private def binding(tree: Tree): Symbol =
      tree.attachments.get[ReifyBindingAttachment] match {
        case Some(ReifyBindingAttachment(binding)) => binding
        case other => NoSymbol
      }

    private val cache = mutable.Map[SymbolTable, List[Tree]]()
    def encode: List[Tree] = cache.getOrElseUpdate(this, SymbolTable.encode(this)) map (_.duplicate)

    override def toString = {
      val symtabString = symtab.keys.map(symName(_)).mkString(", ")
      val trueAliases = aliases.distinct.filter(entry => symName(entry._1) != entry._2)
      val aliasesString = trueAliases.map(entry => s"${symName(entry._1)} -> ${entry._2}").mkString(", ")
      s"""symtab = [$symtabString], aliases = [$aliasesString]${if (original.isDefined) ", has original" else ""}"""
    }

    def debugString: String = {
      val buf = new StringBuilder
      buf.append("symbol table = " + (if (syms.length == 0) "<empty>" else "")).append(EOL)
      syms foreach (sym => buf.append(symDef(sym)).append(EOL))
      buf.delete(buf.length - EOL.length, buf.length)
      buf.toString
    }
  }

  object SymbolTable {
    def apply(): SymbolTable =
      new SymbolTable()

    def apply(encoded: List[Tree]): SymbolTable = {
      var result = new SymbolTable(original = Some(encoded))
      encoded foreach (entry => (entry.attachments.get[ReifyBindingAttachment], entry.attachments.get[ReifyAliasAttachment]) match {
        case (Some(ReifyBindingAttachment(sym)), _) => result += entry
        case (_, Some(ReifyAliasAttachment(sym, alias))) => result = new SymbolTable(result.symtab, result.aliases :+ (sym, alias))
        case _ => // do nothing, this is boilerplate that can easily be recreated by subsequent `result.encode`
      })
      result
    }

    private[SymbolTable] def encode(symtab0: SymbolTable): List[Tree] = {
      if (symtab0.original.isDefined) return symtab0.original.get.map(_.duplicate)
      else assert(hasReifier, "encoding a symbol table requires a reifier")
      // during `encode` we might need to do some reifications
      // these reifications might lead to changes in `reifier.symtab`
      // reifier is mutable, symtab is immutable. this is a tough friendship
      val backup = reifier.state.backup
      reifier.state.symtab = symtab0.asInstanceOf[reifier.SymbolTable]
      def currtab = reifier.symtab.asInstanceOf[SymbolTable]
      try {
        val cumulativeSymtab = mutable.ArrayBuffer[Tree](symtab0.symtab.values.toList: _*)
        val cumulativeAliases = mutable.ArrayBuffer[(Symbol, TermName)](symtab0.aliases: _*)

        def fillInSymbol(sym: Symbol): Tree = {
          if (reifyDebug) println("Filling in: %s (%s)".format(sym, sym.accurateKindString))
          val isFree = currtab.symName(sym) startsWith nme.REIFY_FREE_PREFIX
          if (isFree) {
            if (sym.annotations.isEmpty) EmptyTree
            else Apply(Select(currtab.symRef(sym), nme.setAnnotations), List(reifier.reify(sym.annotations)))
          } else {
           import scala.reflect.internal.Flags._
           if (sym hasFlag LOCKED) {
             // [Eugene] better to have a symbol without a type signature, than to crash with a CyclicReference
             EmptyTree
           } else {
             val rset = reifier.mirrorBuildCall(nme.setTypeSignature, currtab.symRef(sym), reifier.reify(sym.info))
             if (sym.annotations.isEmpty) rset
             else reifier.mirrorBuildCall(nme.setAnnotations, rset, reifier.mkList(sym.annotations map reifier.reifyAnnotationInfo))
           }
          }
        }

        // `fillInSymbol` might add symbols to `symtab`, that's why this is done iteratively
        var progress = 0
        while (progress < cumulativeSymtab.length) {
          val sym = currtab.binding(cumulativeSymtab(progress))
          if (sym != NoSymbol) {
            val symtabProgress = currtab.symtab.size
            val aliasesProgress = currtab.aliases.length
            val fillIn = fillInSymbol(sym)
            cumulativeSymtab ++= currtab.symtab.values drop symtabProgress
            cumulativeAliases ++= currtab.aliases drop aliasesProgress
            cumulativeSymtab += fillIn
          }
          progress += 1
        }

        val withAliases = cumulativeSymtab flatMap (entry => {
          val result = mutable.ListBuffer[Tree]()
          result += entry
          val sym = currtab.binding(entry)
          if (sym != NoSymbol)
            result ++= cumulativeAliases.distinct filter (alias => alias._1 == sym && alias._2 != currtab.symName(sym)) map (alias => {
              val canonicalName = currtab.symName(sym)
              val aliasName = alias._2
              ValDef(NoMods, aliasName, TypeTree(), Ident(canonicalName)) addAttachment ReifyAliasAttachment(sym, aliasName)
            })
          result.toList
        })

        withAliases.toList
      } finally {
        reifier.state.restore(backup)
      }
    }
  }
}