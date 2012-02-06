/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.tools.nsc
package ast

import symtab._
import Flags._
import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.util.FreshNameCreator
import scala.runtime.ScalaRunTime.{ isAnyVal, isTuple }

/** Given a tree or type, generate a tree that when executed at runtime produces the original tree or type.
 *  See more info in the comments to `reify' in scala.reflect.macro.Context.
 *
 *  @author Martin Odersky
 *  @version 2.10
 */
trait Reifiers { self: Global =>

  def reify(tree: Tree): Tree = {
    if (tree.tpe != null) {
      val saved = printTypings
      try {
        val reifyDebug = settings.Yreifydebug.value
        val debugTrace = util.trace when reifyDebug
        debugTrace("transforming = ")(if (settings.Xshowtrees.value) "\n" + nodePrinters.nodeToString(tree).trim else tree.toString)
        debugTrace("transformed = ") {
          val reifier = new Reifier()
          val untyped = reifier.reifyTopLevel(tree)

          val reifyCopypaste = settings.Yreifycopypaste.value
          if (reifyCopypaste) {
            if (reifyDebug) println("=======================")
            println(reifiedNodeToString(untyped))
            if (reifyDebug) println("=======================")
          }

          untyped
        }
      } finally {
        printTypings = saved
      }
    } else {
      CannotReifyPreTyperTrees(tree)
    }
  }

  class Reifier() {
    import definitions._

    final val scalaPrefix = "scala."
    final val localPrefix = "$local"
    final val memoizerName = "$memo"

    val reifyDebug = settings.Yreifydebug.value

    private val reifiableSyms = mutable.ArrayBuffer[Symbol]() // the symbols that are reified with the tree
    private val symIndex = mutable.HashMap[Symbol, Int]() // the index of a reifiable symbol in `reifiableSyms`
    private var boundSyms = Set[Symbol]() // set of all symbols that are bound in tree to be reified

    /**
     * Generate tree of the form
     *
     *    { val $mr = scala.reflect.runtime.Mirror
     *      $local1 = new TypeSymbol(owner1, NoPosition, name1)
     *      ...
     *      $localN = new TermSymbol(ownerN, NoPositiion, nameN)
     *      $local1.setInfo(tpe1)
     *      ...
     *      $localN.setInfo(tpeN)
     *      $localN.setAnnotations(annotsN)
     *      rtree
     *    }
     *
     *  where
     *
     *   - `$localI` are free type symbols in the environment, as well as local symbols
     *      of refinement types.
     *   - `tpeI` are the info's of `symI`
     *   - `rtree` is code that generates `data` at runtime, maintaining all attributes.
     *   - `data` is typically a tree or a type.
     */
    def reifyTopLevel(data: Any): Tree = {
      val rtree = reify(data)
      Block(mirrorAlias :: reifySymbolTableSetup, rtree)
    }

    private def isLocatable(sym: Symbol) =
      sym.isPackageClass || sym.owner.isClass || sym.isTypeParameter && sym.paramPos >= 0

    private def registerReifiableSymbol(sym: Symbol): Unit =
      if (!(symIndex contains sym)) {
        sym.owner.ownersIterator find (x => !isLocatable(x)) foreach registerReifiableSymbol
        symIndex(sym) = reifiableSyms.length
        reifiableSyms += sym
      }

    // helper methods

    private def localName(sym: Symbol): TermName =
      newTermName(localPrefix + symIndex(sym))

    private def call(fname: String, args: Tree*): Tree =
      Apply(termPath(fname), args.toList)

    private def mirrorSelect(name: String): Tree =
      termPath(nme.MIRROR_PREFIX + name)

    private def mirrorCall(name: TermName, args: Tree*): Tree =
      call("" + (nme.MIRROR_PREFIX append name), args: _*)

    private def mirrorCall(name: String, args: Tree*): Tree =
      call(nme.MIRROR_PREFIX + name, args: _*)

    private def mirrorFactoryCall(value: Product, args: Tree*): Tree =
      mirrorCall(value.productPrefix, args: _*)

    private def scalaFactoryCall(name: String, args: Tree*): Tree =
      call(scalaPrefix + name + ".apply", args: _*)

    private def mkList(args: List[Tree]): Tree =
      scalaFactoryCall("collection.immutable.List", args: _*)

    private def reifyModifiers(m: Modifiers) =
      mirrorCall("modifiersFromInternalFlags", reify(m.flags), reify(m.privateWithin), reify(m.annotations))

    private def reifyAggregate(name: String, args: Any*) =
      scalaFactoryCall(name, (args map reify).toList: _*)

    /**
     * Reify a list
     */
    private def reifyList(xs: List[Any]): Tree =
      mkList(xs map reify)

    /** Reify a name */
    private def reifyName(name: Name) =
      mirrorCall(if (name.isTypeName) "newTypeName" else "newTermName", Literal(Constant(name.toString)))

    private def isFree(sym: Symbol) =
      !(symIndex contains sym)

    /**
     * Reify a reference to a symbol
     */
    private def reifySymRef(sym: Symbol): Tree = {
      symIndex get sym match {
        case Some(idx) =>
          Ident(localName(sym))
        case None =>
          if (sym == NoSymbol)
            mirrorSelect("NoSymbol")
          else if (sym == RootPackage)
            mirrorSelect("definitions.RootPackage")
          else if (sym == RootClass)
            mirrorSelect("definitions.RootClass")
          else if (sym == EmptyPackage)
            mirrorSelect("definitions.EmptyPackage")
          else if (sym.isModuleClass)
            Select(reifySymRef(sym.sourceModule), "moduleClass")
          else if (sym.isStatic && sym.isClass)
            mirrorCall("staticClass", reify(sym.fullName))
          else if (sym.isStatic && sym.isModule)
            mirrorCall("staticModule", reify(sym.fullName))
          else if (isLocatable(sym))
            if (sym.isTypeParameter)
              mirrorCall("selectParam", reify(sym.owner), reify(sym.paramPos))
            else {
              if (reifyDebug) println("locatable: " + sym + " " + sym.isPackageClass + " " + sym.owner + " " + sym.isTypeParameter)
              val rowner = reify(sym.owner)
              val rname = reify(sym.name.toString)
              if (sym.isType)
                mirrorCall("selectType", rowner, rname)
              else if (sym.isMethod && sym.owner.isClass && sym.owner.info.decl(sym.name).isOverloaded) {
                val index = sym.owner.info.decl(sym.name).alternatives indexOf sym
                assert(index >= 0, sym)
                mirrorCall("selectOverloadedMethod", rowner, rname, reify(index))
              } else
                mirrorCall("selectTerm", rowner, rname)
            }
          else {
            if (sym.isTerm) {
              if (reifyDebug) println("Free: " + sym)
              val symtpe = lambdaLift.boxIfCaptured(sym, sym.tpe, erasedTypes = false)
              def markIfCaptured(arg: Ident): Tree =
                if (sym.isCapturedVariable) referenceCapturedVariable(arg) else arg
              mirrorCall("newFreeVar", reify(sym.name.toString), reify(symtpe), markIfCaptured(Ident(sym)))
            } else {
              if (reifyDebug) println("Late local: " + sym)
              registerReifiableSymbol(sym)
              reifySymRef(sym)
            }
          }
      }
    }

    /**
     * reify the creation of a symbol
     */
    private def reifySymbolDef(sym: Symbol): Tree = {
      if (reifyDebug) println("reify sym def " + sym)

      ValDef(NoMods, localName(sym), TypeTree(),
        Apply(
          Select(reify(sym.owner), "newNestedSymbol"),
          List(reify(sym.name), reify(sym.pos), Literal(Constant(sym.flags)))
        )
      )
    }

    /**
     * Generate code to add type and annotation info to a reified symbol
     */
    private def fillInSymbol(sym: Symbol): Tree = {
      val rset = Apply(Select(reifySymRef(sym), nme.setTypeSignature), List(reifyType(sym.info)))
      if (sym.annotations.isEmpty) rset
      else Apply(Select(rset, nme.setAnnotations), List(reify(sym.annotations)))
    }

    /** Reify a scope */
    private def reifyScope(scope: Scope): Tree = {
      scope foreach registerReifiableSymbol
      mirrorCall(nme.newScopeWith, scope.toList map reifySymRef: _*)
    }

    /** Reify a list of symbols that need to be created */
    private def reifySymbols(syms: List[Symbol]): Tree = {
      syms foreach registerReifiableSymbol
      mkList(syms map reifySymRef)
    }

    /** Reify a type that defines some symbols */
    private def reifyTypeBinder(value: Product, bound: List[Symbol], underlying: Type): Tree =
      mirrorFactoryCall(value, reifySymbols(bound), reify(underlying))

    /** Reify a type */
    private def reifyType(tpe0: Type): Tree = {
      val tpe = tpe0.normalize
      val tsym = tpe.typeSymbol
      if (tsym.isClass && tpe == tsym.typeConstructor && tsym.isStatic)
        Select(reifySymRef(tpe.typeSymbol), nme.asTypeConstructor)
      else tpe match {
        case t @ NoType =>
          reifyMirrorObject(t)
        case t @ NoPrefix =>
          reifyMirrorObject(t)
        case tpe @ ThisType(clazz) if clazz.isModuleClass && clazz.isStatic =>
          mirrorCall(nme.thisModuleType, reify(clazz.fullName))
        case t @ RefinedType(parents, decls) =>
          registerReifiableSymbol(tpe.typeSymbol)
          mirrorFactoryCall(t, reify(parents), reify(decls), reify(t.typeSymbol))
        case t @ ClassInfoType(parents, decls, clazz) =>
          registerReifiableSymbol(clazz)
          mirrorFactoryCall(t, reify(parents), reify(decls), reify(t.typeSymbol))
        case t @ ExistentialType(tparams, underlying) =>
          reifyTypeBinder(t, tparams, underlying)
        case t @ PolyType(tparams, underlying) =>
          reifyTypeBinder(t, tparams, underlying)
        case t @ MethodType(params, restpe) =>
          reifyTypeBinder(t, params, restpe)
        case _ =>
          reifyProductUnsafe(tpe)
      }
    }

    private def definedInLiftedCode(tpe: Type) =
      tpe exists (tp => boundSyms contains tp.typeSymbol)

    private def isErased(tree: Tree) = tree match {
      case tt: TypeTree => definedInLiftedCode(tt.tpe) && tt.original == null
      case _ => false
    }

    /** Reify a tree */
    private def reifyTree(tree: Tree): Tree = tree match {
      case EmptyTree =>
        reifyMirrorObject(EmptyTree)
      case This(_) if tree.symbol != NoSymbol && !(boundSyms contains tree.symbol) =>
        reifyFree(tree)
      case Ident(_) if tree.symbol != NoSymbol && !(boundSyms contains tree.symbol) =>
        if (tree.symbol.isVariable && tree.symbol.owner.isTerm) {
          captureVariable(tree.symbol) // Note order dependency: captureVariable needs to come before reifyTree here.
          mirrorCall("Select", reifyFree(tree), reifyName(nme.elem))
        } else reifyFree(tree)
      case tt: TypeTree if (tt.tpe != null) =>
        if (definedInLiftedCode(tt.tpe)) {
          // erase non-essential (i.e. inferred) types
          // reify symless counterparts of essential types
          // @xeno.by: in general case reflective compiler lacks the context to typecheck the originals
          // more info here: https://issues.scala-lang.org/browse/SI-5273?focusedCommentId=56057#comment-56057
          // this is A BIG BAD TODO!
          if (tt.original != null) reify(tt.original) else mirrorCall("TypeTree")
        } else {
          var rtt = mirrorCall(nme.TypeTree, reifyType(tt.tpe))
          // @xeno.by: originals get typechecked during subsequent reflective compilation, which leads to subtle bugs
          // https://issues.scala-lang.org/browse/SI-5273?focusedCommentId=56057#comment-56057
          // until this is somehow sorted out, I disable reification of originals
          // if (tt.original != null) {
          //   val setOriginal = Select(rtt, newTermName("setOriginal"))
          //   val reifiedOriginal = reify(tt.original)
          //   rtt = Apply(setOriginal, List(reifiedOriginal))
          // }
          rtt
        }
      case ta @ TypeApply(hk, ts) =>
        if (ts exists isErased) reifyTree(hk) else reifyProduct(ta)
      case self.emptyValDef =>
        mirrorSelect(nme.emptyValDef)
      case Literal(constant @ Constant(tpe: Type)) if boundSyms exists (tpe contains _) =>
        CannotReifyClassOfBoundType(tree, tpe)
      case Literal(constant @ Constant(sym: Symbol)) if boundSyms contains sym =>
        CannotReifyClassOfBoundEnum(tree, constant.tpe)
      case _ =>
        if (tree.isDef) {
          if (reifyDebug) println("boundSym: " + tree.symbol)
          boundSyms += tree.symbol
        }

        reifyProduct(tree)
      /*
          if (tree.isDef || tree.isInstanceOf[Function])
            registerReifiableSymbol(tree.symbol)
          if (tree.hasSymbol)
            rtree = Apply(Select(rtree, nme.setSymbol), List(reifySymRef(tree.symbol)))
          Apply(Select(rtree, nme.setType), List(reifyType(tree.tpe)))
*/
    }

    /**
     * Reify a free reference. The result will be either a mirror reference
     *  to a global value, or else a mirror Literal.
     */
    private def reifyFree(tree: Tree): Tree = tree match {
      case This(_) if tree.symbol.isClass && !tree.symbol.isModuleClass =>
        val sym = tree.symbol
        if (reifyDebug) println("This for %s, reified as freeVar".format(sym))
        if (reifyDebug) println("Free: " + sym)
        val freeVar = mirrorCall("newFreeVar", reify(sym.name.toString), reify(sym.tpe), This(sym))
        mirrorCall(nme.Ident, freeVar)
      case This(_) =>
        if (reifyDebug) println("This for %s, reified as This".format(tree.symbol))
        mirrorCall(nme.This, reifySymRef(tree.symbol))
      case _ =>
        mirrorCall(nme.Ident, reifySymRef(tree.symbol))
    }

    // todo: consider whether we should also reify positions
    private def reifyPosition(pos: Position): Tree =
      reifyMirrorObject(NoPosition)

    // !!! we must eliminate these casts.
    private def reifyProductUnsafe(x: Any): Tree =
      reifyProduct(x.asInstanceOf[Product])
    private def reifyProduct(x: Product): Tree =
      mirrorCall(x.productPrefix, (x.productIterator map reify).toList: _*)

    /**
     * Reify a case object defined in Mirror
     */
    private def reifyMirrorObject(name: String): Tree = mirrorSelect(name)
    private def reifyMirrorObject(x: Product): Tree   = reifyMirrorObject(x.productPrefix)

    private def isReifiableConstant(value: Any) = value match {
      case null      => true  // seems pretty reifable to me?
      case _: String => true
      case _         => isAnyVal(value)
    }

    /** Reify an arbitary value */
    private def reify(value: Any): Tree = value match {
      case tree: Tree   => reifyTree(tree)
      case sym: Symbol  => reifySymRef(sym)
      case tpe: Type    => reifyType(tpe)
      case xs: List[_]  => reifyList(xs)
      case xs: Array[_] => scalaFactoryCall(nme.Array, xs map reify: _*)
      case scope: Scope => reifyScope(scope)
      case x: Name      => reifyName(x)
      case x: Position  => reifyPosition(x)
      case x: Modifiers => reifyModifiers(x)
      case _ =>
        if (isReifiableConstant(value)) Literal(Constant(value))
        else reifyProductUnsafe(value)
    }

    /**
     * An (unreified) path that refers to definition with given fully qualified name
     *  @param mkName   Creator for last portion of name (either TermName or TypeName)
     */
    private def path(fullname: String, mkName: String => Name): Tree = {
      val parts = fullname split "\\."
      val prefixParts = parts.init
      val lastName = mkName(parts.last)
      if (prefixParts.isEmpty) Ident(lastName)
      else {
        val prefixTree = ((Ident(prefixParts.head): Tree) /: prefixParts.tail)(Select(_, _))
        Select(prefixTree, lastName)
      }
    }

    /** An (unreified) path that refers to term definition with given fully qualified name */
    private def termPath(fullname: String): Tree = path(fullname, newTermName)

    /** An (unreified) path that refers to type definition with given fully qualified name */
    private def typePath(fullname: String): Tree = path(fullname, newTypeName)

    private def mirrorAlias =
      ValDef(NoMods, nme.MIRROR_SHORT, SingletonTypeTree(termPath(fullnme.MirrorPackage)), termPath(fullnme.MirrorPackage))

    /**
     * Generate code that generates a symbol table of all symbols registered in `reifiableSyms`
     */
    private def reifySymbolTableSetup: List[Tree] = {
      val symDefs, fillIns = new mutable.ArrayBuffer[Tree]
      var i = 0
      while (i < reifiableSyms.length) {
        // fillInSymbol might create new reifiableSyms, that's why this is done iteratively
        symDefs += reifySymbolDef(reifiableSyms(i))
        fillIns += fillInSymbol(reifiableSyms(i))
        i += 1
      }

      symDefs.toList ++ fillIns.toList
    }
  }

  /** A throwable signalling a reification error */
  class ReifierError(var pos: Position, val msg: String) extends Throwable(msg) {
    def this(msg: String) = this(NoPosition, msg)
  }

  def CannotReifyPreTyperTrees(tree: Tree) = {
    val msg = "pre-typer trees are not supported, consider typechecking the tree before passing it to the reifier"
    throw new ReifierError(tree.pos, msg)
  }

  def CannotReifyClassOfBoundType(tree: Tree, tpe: Type) = {
    val msg = "cannot reify classOf[%s] which refers to a type declared inside the block being reified".format(tpe)
    throw new ReifierError(tree.pos, msg)
  }

  def CannotReifyClassOfBoundEnum(tree: Tree, tpe: Type) = {
    val msg = "cannot reify classOf[%s] which refers to an enum declared inside the block being reified".format(tpe)
    throw new ReifierError(tree.pos, msg)
  }
}
