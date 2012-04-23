package scala.reflect.reify
package codegen

trait Symbols {
  self: Reifier =>

  import mirror._
  import definitions._
  import treeInfo._

  /** Reify a reference to a symbol */
  def reifySymRef(sym0: Symbol): Tree = {
    assert(sym0 != null, "sym is null")
    val sym = sym0.dealias

    if (sym == NoSymbol)
      mirrorSelect(nme.NoSymbol)
    else if (sym == RootPackage)
      Select(mirrorSelect(nme.definitions), nme.RootPackage)
    else if (sym == RootClass)
      Select(mirrorSelect(nme.definitions), nme.RootClass)
    else if (sym == EmptyPackage)
      Select(mirrorSelect(nme.definitions), nme.EmptyPackage)
    else if (sym == EmptyPackageClass)
      Select(mirrorSelect(nme.definitions), nme.EmptyPackageClass)
    else if (sym.isModuleClass)
      Select(reify(sym.sourceModule), nme.moduleClass)
    else if (sym.isLocatable) {
      // [Eugene] am I doing this right?
//      if (sym.isStaticOwner) { // no good for us, because it returns false for packages
      if (sym.isStatic && (sym.isClass || sym.isModule)) {
        val resolver = if (sym.isType) nme.staticClass else nme.staticModule
        mirrorCall(resolver, reify(sym.fullName))
      } else {
        if (reifyDebug) println("Locatable: %s (%s) owned by %s (%s) at %s".format(sym, sym.accurateKindString, sym.owner, sym.owner.accurateKindString, sym.owner.fullNameString))
        val rowner = reify(sym.owner)
        val rname = reify(sym.name.toString)
        if (sym.isType)
          mirrorCall(nme.selectType, rowner, rname)
        else if (sym.isMethod && sym.owner.isClass && sym.owner.info.decl(sym.name).isOverloaded) {
          val index = sym.owner.info.decl(sym.name).alternatives indexOf sym
          assert(index >= 0, sym)
          mirrorCall(nme.selectOverloadedMethod, rowner, rname, reify(index))
        } else
          mirrorCall(nme.selectTerm, rowner, rname)
      }
    } else {
      // todo. make sure that free methods and free local defs work correctly
      if (sym.isTerm) reifyFreeTerm(sym, Ident(sym))
      else reifyFreeType(sym, Ident(sym))
    }
  }

  def reifyFreeTerm(sym: Symbol, value: Tree): Tree =
    locallyReified get sym match {
      case Some(reified) =>
        reified
      case None =>
        if (reifyDebug) println("Free term" + (if (sym.isCapturedVariable) " (captured)" else "") + ": " + sym + "(" + sym.accurateKindString + ")")
        var name = newTermName(nme.MIRROR_FREE_PREFIX + sym.name)
        if (sym.isType) name = name.append(nme.MIRROR_FREE_THIS_SUFFIX)
        if (sym.isCapturedVariable) {
          assert(value.isInstanceOf[Ident], showRaw(value))
          val capturedTpe = capturedVariableType(sym)
          val capturedValue = referenceCapturedVariable(sym)
          locallyReify(sym, name, mirrorCall(nme.newFreeTerm, reify(sym.name.toString), reify(capturedTpe), capturedValue, reify(sym.flags), reify(origin(sym))))
        } else {
          locallyReify(sym, name, mirrorCall(nme.newFreeTerm, reify(sym.name.toString), reify(sym.tpe), value, reify(sym.flags), reify(origin(sym))))
        }
    }

  def reifyFreeType(sym: Symbol, value: Tree): Tree =
    locallyReified get sym match {
      case Some(reified) =>
        reified
      case None =>
        if (reifyDebug) println("Free type: %s (%s)".format(sym, sym.accurateKindString))
        var name = newTermName(nme.MIRROR_FREE_PREFIX + sym.name)
        val phantomTypeTag = Apply(TypeApply(Select(Ident(nme.MIRROR_SHORT), nme.TypeTag), List(value)), List(Literal(Constant(null)), Literal(Constant(null))))
        val flavor = if (sym.isExistential) nme.newFreeExistential else nme.newFreeType
        locallyReify(sym, name, mirrorCall(flavor, reify(sym.name.toString), reify(sym.info), phantomTypeTag, reify(sym.flags), reify(origin(sym))))
    }

  def reifySymDef(sym: Symbol): Tree =
    locallyReified get sym match {
      case Some(reified) =>
        reified
      case None =>
        if (reifyDebug) println("Sym def: %s (%s)".format(sym, sym.accurateKindString))
        assert(!sym.isLocatable, sym) // if this assertion fires, then tough type reification needs to be rethought
        sym.owner.ownersIterator find (!_.isLocatable) foreach reifySymDef
        var name = newTermName(nme.MIRROR_SYMDEF_PREFIX + sym.name)
        locallyReify(sym, name, Apply(Select(reify(sym.owner), nme.newNestedSymbol), List(reify(sym.name), reify(sym.pos), reify(sym.flags), reify(sym.isClass))))
    }

  // todo. very brittle abstraction, needs encapsulation
  import scala.collection.mutable._
  private val localReifications = ArrayBuffer[Tree]()
  private val locallyReified = Map[Symbol, Tree]()
  private var filledIn = false
  def symbolTable: List[Tree] = { fillInSymbolTable(); localReifications.toList }
  def symbolTable_=(newSymbolTable: List[Tree]): Unit = {
    localReifications.clear()
    locallyReified.clear()
    filledIn = false
    newSymbolTable foreach {
      case entry =>
        val att = entry.attachmentOpt[ReifyAttachment]
        att match {
          case Some(ReifyAttachment(sym)) =>
            // don't duplicate reified symbols when merging inlined reifee
            if (!(locallyReified contains sym)) {
              val ValDef(_, name, _, _) = entry
              localReifications += entry
              locallyReified(sym) = Ident(name)
            }
          case other =>
            // do nothing => symbol table fill-ins will be repopulated later
        }
    }
  }

  private def localName(name0: TermName): TermName = {
    var name = name0.toString
    name = name.replace(".type", "$type")
    name = name.replace(" ", "$")
    val fresh = typer.context.unit.fresh
    newTermName(fresh.newName(name))
  }

  private def locallyReify(sym: Symbol, name0: TermName, reificode: => Tree): Tree = {
    val reified = reificode
    val name = localName(name0)
    // todo. tried to declare a private class here to carry an attachment, but it's path-dependent
    // so got troubles with exchanging free variables between nested and enclosing quasiquotes
    // attaching just Symbol isn't good either, so we need to think of a principled solution
    val local = ValDef(NoMods, name, TypeTree(), reified) withAttachment ReifyAttachment(sym)
    localReifications += local
    filledIn = false
    locallyReified(sym) = Ident(name)
    locallyReified(sym)
  }

  /** Sets type signatures and annotations for locally reified symbols */
  private def fillInSymbolTable() = {
    if (!filledIn) {
      val fillIns = new ArrayBuffer[Tree]
      var i = 0
      while (i < localReifications.length) {
        // fillInSymbol might create new locallyReified symbols, that's why this is done iteratively
        val reified = localReifications(i)
        val att = reified.attachmentOpt[ReifyAttachment]
        att match {
          case Some(ReifyAttachment(sym)) => fillIns += fillInSymbol(sym)
          case other => // do nothing
        }
        i += 1
      }

      filledIn = true
      localReifications ++= fillIns.toList
    }
  }

  /** Generate code to add type and annotation info to a reified symbol */
  private def fillInSymbol(sym: Symbol): Tree = {
    if (reifyDebug) println("Filling in: %s (%s)".format(sym, sym.accurateKindString))
    val isFree = locallyReified(sym) match { case Ident(name) => name startsWith nme.MIRROR_FREE_PREFIX }
    if (isFree) {
      if (sym.annotations.isEmpty) EmptyTree
      else Apply(Select(locallyReified(sym), nme.setAnnotations), List(reify(sym.annotations)))
    } else {
     import scala.reflect.internal.Flags._
     if (sym hasFlag LOCKED) {
       // [Eugene] better to have a symbol without a type signature, than to crash with a CyclicReference
       EmptyTree
     } else {
       val rset = Apply(Select(locallyReified(sym), nme.setTypeSignature), List(reify(sym.info)))
       if (sym.annotations.isEmpty) rset
       else Apply(Select(rset, nme.setAnnotations), List(reify(sym.annotations)))
     }
    }
  }
}