/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.util.FreshNameCreator
import scala.runtime.ScalaRunTime.{ isAnyVal, isTuple }

/**
 * Translate expressions of the form reflect.Code.lift(exp)
 *  to the reified "reflect trees" representation of exp.
 *  Also: mutable variables that are accessed from a local function are wrapped in refs.
 *
 *  @author Martin Odersky
 *  @version 2.10
 */
abstract class LiftCode extends Transform with TypingTransformers {

  import global._ // the global environment
  import definitions._ // standard classes and methods
  import typer.{ typed, atOwner } // methods to type trees

  val symbols: global.type = global

  /** the following two members override abstract members in Transform */
  val phaseName: String = "liftcode"

  def newTransformer(unit: CompilationUnit): Transformer =
    new Codifier(unit)

  private lazy val MirrorMemberNames =
    ReflectRuntimeMirror.info.nonPrivateMembers filter (_.isTerm) map (_.toString) toSet

  // Would be nice if we could use something like this to check the names,
  // but it seems that info is unavailable when I need it.
  private def mirrorFactoryName(value: Any): Option[String] = value match {
    // Modest (inadequate) sanity check that there's a member by this name.
    case x: Product if MirrorMemberNames(x.productPrefix) =>
      Some(x.productPrefix)
    case _ =>
      Some(value.getClass.getName split """[$.]""" last) filter MirrorMemberNames
  }
  private def isMirrorMemberObject(value: Product) = value match {
    case NoType | NoPrefix | NoPosition | EmptyTree => true
    case _                                          => false
  }

  class Codifier(unit: CompilationUnit) extends TypingTransformer(unit) {

    val reifyDebug = settings.Yreifydebug.value
    val debugTrace = util.trace when reifyDebug

    val reifyCopypaste = settings.Yreifycopypaste.value
    def printCopypaste(tree: Tree) {
      import scala.reflect.api.Modifier
      import scala.reflect.api.Modifier._

      def copypasteModifier(mod: Modifier.Value): String = mod match {
        case mod @ (
             `protected` | `private` | `override` |
             `abstract` | `final` | `sealed` |
             `implicit` | `lazy` | `macro` |
             `case` | `trait`) => "`" + mod.toString + "`"
        case mod => mod.toString
      }

      // I fervently hope this is a test case or something, not anything being
      // depended upon.  Of more fragile code I cannot conceive.
      for (line <- (tree.toString.split(Properties.lineSeparator) drop 2 dropRight 1)) {
        var s = line.trim
        s = s.replace("$mr.", "")
        s = s.replace(".apply", "")
        s = s.replace("scala.collection.immutable.", "")
        s = "List\\[List\\[.*?\\].*?\\]".r.replaceAllIn(s, "List")
        s = "List\\[.*?\\]".r.replaceAllIn(s, "List")
        s = s.replace("immutable.this.Nil", "List()")
        s = s.replace("modifiersFromInternalFlags", "Modifiers")
        s = s.replace("Modifiers(0L, newTypeName(\"\"), List())", "Modifiers()")
        s = """Modifiers\((\d+)[lL], newTypeName\("(.*?)"\), List\((.*?)\)\)""".r.replaceAllIn(s, m => {
          val buf = new StringBuilder

          val flags = m.group(1).toLong
          val s_flags = Flags.modifiersOfFlags(flags) map copypasteModifier mkString ", "
          if (s_flags != "")
            buf.append("Set(" + s_flags + ")")

          val privateWithin = "" + m.group(2)
          if (privateWithin != "")
            buf.append(", newTypeName(\"" + privateWithin + "\")")

          val annotations = m.group(3)
          if (annotations.nonEmpty)
            buf.append(", List(" + annotations + ")")

          "Modifiers(" + buf.toString  + ")"
        })
        s = """setInternalFlags\((\d+)L\)""".r.replaceAllIn(s, m => {
          val flags = m.group(1).toLong
          val mods = Flags.modifiersOfFlags(flags) map copypasteModifier
          "setInternalFlags(flagsOfModifiers(List(" + mods.mkString(", ") + ")))"
        })

        println(s)
      }
    }

    override def transformUnit(unit: CompilationUnit) {
      atPhase(phase.next) {
        super.transformUnit(unit)
      }
    }

    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
        case Apply(_, List(tree)) if sym == Code_lift => // reify Code.lift[T](expr) instances
          val saved = printTypings
          try {
            printTypings = reifyDebug
            debugTrace("transformed = ") {
              val result = localTyper.typedPos(tree.pos)(codify(super.transform(tree)))
              if (reifyCopypaste) printCopypaste(result)
              result
            }
          } catch {
            case ex: ReifierError =>
              unit.error(ex.pos, ex.msg)
              tree
          } finally {
            printTypings = saved
          }
        case _ =>
          super.transform(tree)
      }
    }

    def codify(tree: Tree): Tree = debugTrace("codified " + tree + " -> ") {
      val targetType = definitions.CodeClass.primaryConstructor.info.paramTypes.head
      val reifier = new Reifier()
      val arg = gen.mkAsInstanceOf(reifier.reifyTopLevel(tree), targetType, wrapInApply = false)
      val treetpe =
        if (tree.tpe.typeSymbol.isAnonymousClass) tree.tpe.typeSymbol.classBound
        else tree.tpe
      New(TypeTree(appliedType(definitions.CodeClass.typeConstructor, List(treetpe.widen))),
        List(List(arg)))
    }
  }

  /**
   *  Given a tree or type, generate a tree that when executed at runtime produces the original tree or type.
   *  For instance: Given
   *
   *   var x = 1; Code(x + 1)
   *
   *  The `x + 1` expression is reified to
   *
   *  $mr.Apply($mr.Select($mr.Ident($mr.freeVar("x". <Int>, x), "+"), List($mr.Literal($mr.Constant(1))))))
   *
   *  Or, the term name 'abc' is reified to:
   *
   *  $mr.Apply($mr.Select($mr.Ident("newTermName")), List(Literal(Constant("abc")))))
   *
   * todo: Treat embedded Code blocks by merging them into containing block
   *
   */
  class Reifier() {

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
              mirrorCall("freeVar", reify(sym.name.toString), reify(symtpe), markIfCaptured(Ident(sym)))
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
      val rset = Apply(Select(reifySymRef(sym), nme.setTypeSig), List(reifyType(sym.info)))
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

    /** Reify a tree */
    private def reifyTree(tree: Tree): Tree = tree match {
      case EmptyTree =>
        reifyMirrorObject(EmptyTree)
      case This(_) if !(boundSyms contains tree.symbol) =>
        reifyFree(tree)
      case Ident(_) if !(boundSyms contains tree.symbol) =>
        if (tree.symbol.isVariable && tree.symbol.owner.isTerm) {
          captureVariable(tree.symbol) // Note order dependency: captureVariable needs to come before reifyTree here.
          mirrorCall("Select", reifyFree(tree), reifyName(nme.elem))
        } else reifyFree(tree)
      case tt: TypeTree if (tt.tpe != null) =>
        if (!(boundSyms exists (tt.tpe contains _))) mirrorCall("TypeTree", reifyType(tt.tpe))
        else if (tt.original != null) reify(tt.original)
        else mirrorCall(nme.TypeTree)
      case ta @ TypeApply(hk, ts) =>
        val thereAreOnlyTTs = ts collect { case t if !t.isInstanceOf[TypeTree] => t } isEmpty;
        val ttsAreNotEssential = ts collect { case tt: TypeTree => tt } find { tt => tt.original != null } isEmpty;
        if (thereAreOnlyTTs && ttsAreNotEssential) reifyTree(hk) else reifyProduct(ta)
      case global.emptyValDef =>
        mirrorSelect(nme.emptyValDef)
      case Literal(constant @ Constant(tpe: Type)) if boundSyms exists (tpe contains _) =>
        CannotReifyClassOfBoundType(tree, tpe)
      case Literal(constant @ Constant(sym: Symbol)) if boundSyms contains sym =>
        CannotReifyClassOfBoundEnum(tree, constant.tpe)
      case _ =>
        if (tree.isDef)
          boundSyms += tree.symbol

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
    private def reifyFree(tree: Tree): Tree =
      mirrorCall(nme.Ident, reifySymRef(tree.symbol))

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
      ValDef(NoMods, nme.MIRROR_SHORT, TypeTree(), termPath(fullnme.MirrorPackage))

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

  def CannotReifyClassOfBoundType(tree: Tree, tpe: Type) = {
    val msg = "cannot reify classOf[%s] which refers to a type declared inside the block being reified".format(tpe)
    throw new ReifierError(tree.pos, msg)
  }

  def CannotReifyClassOfBoundEnum(tree: Tree, tpe: Type) = {
    val msg = "cannot reify classOf[%s] which refers to an enum declared inside the block being reified".format(tpe)
    throw new ReifierError(tree.pos, msg)
  }
}
