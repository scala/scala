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

/** Translate expressions of the form reflect.Code.lift(exp)
 *  to the reified "reflect trees" representation of exp.
 *  Also: mutable variables that are accessed from a local function are wrapped in refs.
 *
 *  @author Martin Odersky
 *  @version 2.10
 */
abstract class LiftCode extends Transform with TypingTransformers {

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed, atOwner}    // methods to type trees

  val symbols: global.type = global

  /** the following two members override abstract members in Transform */
  val phaseName: String = "liftcode"

  def newTransformer(unit: CompilationUnit): Transformer =
    new Codifier(unit)

  class Codifier(unit: CompilationUnit) extends TypingTransformer(unit) {

    val reifyDebug = settings.Yreifydebug.value
    val debugTrace = util.trace when reifyDebug

    /** Set of mutable local variables that are free in some inner method. */
    private val freeMutableVars: mutable.Set[Symbol] = new mutable.HashSet
    private val converted: mutable.Set[Symbol] = new mutable.HashSet // debug

    override def transformUnit(unit: CompilationUnit) {
      freeMutableVars.clear()
      freeLocalsTraverser(unit.body)
      atPhase(phase.next) {
        super.transformUnit(unit)
      }
      for (v <- freeMutableVars) //!!! remove
        assert(converted contains v, "unconverted: "+v+" in "+v.owner+" in unit "+unit)
    }

    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
        case Apply(_, List(tree)) if sym == Code_lift => // reify Code.lift[T](expr) instances
          val saved = printTypings
          try {
            printTypings = reifyDebug
            debugTrace("transformed = ") {
              localTyper.typedPos(tree.pos)(codify(super.transform(tree)))
            }
          } finally printTypings = saved
        case ValDef(mods, name, tpt, rhs) if (freeMutableVars(sym)) => // box mutable variables that are accessed from a local closure
          val tpt1 = TypeTree(sym.tpe) setPos tpt.pos
          /* Creating a constructor argument if one isn't present. */
          val constructorArg = rhs match {
            case EmptyTree => gen.mkZero(atPhase(phase.prev)(sym.tpe))
            case _ => transform(rhs)
          }
          val rhs1 = typer.typedPos(rhs.pos) {
            Apply(Select(New(TypeTree(sym.tpe)), nme.CONSTRUCTOR), List(constructorArg))
          }
          sym resetFlag MUTABLE
          sym removeAnnotation VolatileAttr
          converted += sym// dereference boxed variables
          treeCopy.ValDef(tree, mods &~ MUTABLE, name, tpt1, rhs1)
        case Ident(name) if freeMutableVars(sym) =>
          localTyper.typedPos(tree.pos) {
            Select(tree setType sym.tpe, nme.elem)
          }
        case _ =>
          super.transform(tree)
      }
    }

    /** todo: Treat embedded Code blocks by merging them into containing block
     *
     */
    class Reifier() {

      final val mirrorFullName = "scala.reflect.runtime.Mirror"
      final val mirrorShortName = "$mr"
      final val mirrorPrefix = mirrorShortName + "."
      final val scalaPrefix = "scala."
      final val localPrefix = "$local"
      final val memoizerName = "$memo"

      private val reifiableSyms = mutable.ArrayBuffer[Symbol]() // the symbols that are reified with the tree
      private val symIndex = mutable.HashMap[Symbol, Int]() // the index of a reifiable symbol in `reifiableSyms`
      private var boundSyms = Set[Symbol]() // set of all symbols that are bound in tree to be reified

      /** Generate tree of the form
       *
       *    { val $mr = scala.reflect.runtime.Mirror
       *      val $memo = new scala.reflext.runtime.Memoizer
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
       *   - `rtree` is code that generates `tree` at runtime, maintaining all attributes.
       */
      def reifyTopLevel(tree: Tree): Tree = {
        val rtree = reify(tree)
        Block(mirrorAlias :: reifySymbolTableSetup, rtree)
      }

      private def isLocatable(sym: Symbol) =
        sym.isPackageClass || sym.owner.isClass || sym.isTypeParameter && sym.paramPos >= 0

      private def registerReifiableSymbol(sym: Symbol): Unit =
        if (!(symIndex contains sym)) {
          sym.owner.ownersIterator.find(!isLocatable(_)) match {
            case Some(outer) => registerReifiableSymbol(outer)
            case None =>
          }
          symIndex(sym) = reifiableSyms.length
          reifiableSyms += sym
        }

      // helper methods

      private def localName(sym: Symbol) = localPrefix + symIndex(sym)

      private def call(fname: String, args: Tree*): Tree =
        Apply(termPath(fname), args.toList)

      private def mirrorSelect(name: String): Tree =
        termPath(mirrorPrefix + name)

      private def mirrorCall(name: String, args: Tree*): Tree =
        call(mirrorPrefix + name, args: _*)

      private def mirrorFactoryCall(value: Product, args: Tree*): Tree =
        mirrorCall(value.productPrefix, args: _*)

      private def scalaFactoryCall(name: String, args: Tree*): Tree =
        call(scalaPrefix + name + ".apply", args: _*)

      private def mkList(args: List[Tree]): Tree =
        scalaFactoryCall("collection.immutable.List", args: _*)

      /** Reify a case object defined in Mirror
       */
      private def reifyCaseObject(value: Product) = mirrorSelect(value.productPrefix)

      /** Reify an instance of a case class defined in Mirror
       */
      private def reifyCaseClassInstance(value: Product) =
        mirrorFactoryCall(value, (value.productIterator map reify).toList: _*)

      private def reifyAggregate(name: String, args: Any*) =
        scalaFactoryCall(name, (args map reify).toList: _*)

      /** Reify a list
       */
      private def reifyList(xs: List[Any]): Tree =
        mkList(xs map reify)

      /** Reify a name */
      private def reifyName(name: Name) =
        mirrorCall(if (name.isTypeName) "newTypeName" else "newTermName", Literal(Constant(name.toString)))

      private def isFree(sym: Symbol) =
        !(symIndex contains sym)

      /** Reify a reference to a symbol
       */
      private def reifySymRef(sym: Symbol): Tree = {
        symIndex get sym match {
          case Some(idx) =>
            Ident(localName(sym))
          case None =>
            if (sym == NoSymbol)
              mirrorSelect("NoSymbol")
            else  if (sym.isModuleClass)
              Select(reifySymRef(sym.sourceModule), "moduleClass")
            else if (sym.isStatic && sym.isClass)
              mirrorCall("staticClass", reify(sym.fullName))
            else if (sym.isStatic && sym.isModule)
              mirrorCall("staticModule", reify(sym.fullName))
            else if (isLocatable(sym))
              if (sym.isTypeParameter)
                mirrorCall("selectParam", reify(sym.owner), reify(sym.paramPos))
              else {
                if (reifyDebug) println("locatable: "+sym+" "+sym.isPackageClass+" "+sym.owner+" "+sym.isTypeParameter)
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
                if (reifyDebug) println("Free: "+sym)
                mirrorCall("freeVar", reify(sym.name.toString), reify(sym.tpe), Ident(sym))
              } else {
                if (reifyDebug) println("Late local: "+sym)
                registerReifiableSymbol(sym)
                reifySymRef(sym)
              }
            }
        }
      }

      /** reify the creation of a symbol
       */
      private def reifySymbolDef(sym: Symbol): Tree = {
        if (reifyDebug) println("reify sym def "+sym)
        var rsym: Tree = New(
            typePath(mirrorPrefix + (if (sym.isType) "TypeSymbol" else "TermSymbol")),
            List(List(reify(sym.owner), reify(sym.pos), reify(sym.name))))
        if (sym.flags != 0L)
          rsym = Apply(Select(rsym, "setFlag"), List(Literal(Constant(sym.flags))))
        ValDef(NoMods, localName(sym), TypeTree(), rsym)
      }

      /** Generate code to add type and annotation info to a reified symbol
       */
      private def fillInSymbol(sym: Symbol): Tree = {
        val rset = Apply(Select(reifySymRef(sym), "setInfo"), List(reifyType(sym.info)))
        if (sym.annotations.isEmpty) rset
        else Apply(Select(rset, "setAnnotations"), List(reify(sym.annotations)))
      }

      /** Reify a scope */
      private def reifyScope(scope: Scope): Tree = {
        scope foreach registerReifiableSymbol
        mirrorCall("newScopeWith", scope.toList map reifySymRef: _*)
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
          Select(reifySymRef(tpe.typeSymbol), "typeConstructor")
        else tpe match {
          case NoType | NoPrefix =>
            reifyCaseObject(tpe.asInstanceOf[Product])
          case tpe @ ThisType(clazz) =>
            if (clazz.isModuleClass && clazz.isStatic) mirrorCall("thisModuleType", reify(clazz.fullName))
            else reifyCaseClassInstance(tpe)
          case SuperType(_, _) | SingleType(_, _) | ConstantType(_) |
            TypeRef(_, _, _) | AnnotatedType(_, _, _) |
            TypeBounds(_, _) | NullaryMethodType(_) | OverloadedType(_, _) =>
            reifyCaseClassInstance(tpe.asInstanceOf[Product])
          case t @ RefinedType(parents, decls) =>
            registerReifiableSymbol(tpe.typeSymbol)
            mirrorFactoryCall(t, reify(parents), reify(decls), reify(t.typeSymbol))
          case t @ ExistentialType(tparams, underlying) =>
            reifyTypeBinder(t, tparams, underlying)
          case t @ PolyType(tparams, underlying) =>
            reifyTypeBinder(t, tparams, underlying)
          case t @ MethodType(params, restpe) =>
            reifyTypeBinder(t, params, restpe)
          case _ =>
            abort("cannot reify type " + tpe + " of class " + tpe.getClass)
        }
      }

      /** Reify a tree */
      private def reifyTree(tree: Tree): Tree = tree match {
        case EmptyTree =>
          reifyCaseObject(tree)
        case This(_) if !(boundSyms contains tree.symbol) =>
          reifyFree(tree)
        case Ident(_) if !(boundSyms contains tree.symbol) =>
          reifyFree(tree)
        case TypeTree() if (tree.tpe != null) =>
          mirrorCall("TypeTree", reifyType(tree.tpe))
        case _ =>
          if (tree.isDef) boundSyms += tree.symbol
          reifyCaseClassInstance(tree.asInstanceOf[Product])
/*
          if (tree.isDef || tree.isInstanceOf[Function])
            registerReifiableSymbol(tree.symbol)
          if (tree.hasSymbol)
            rtree = Apply(Select(rtree, "setSymbol"), List(reifySymRef(tree.symbol)))
          Apply(Select(rtree, "setType"), List(reifyType(tree.tpe)))
*/
      }

      /** Reify a free reference. The result will be either a mirror reference
       *  to a global value, or else a mirror Literal.
       */
      private def reifyFree(tree: Tree): Tree =
        mirrorCall("Ident", reifySymRef(tree.symbol))

      /** Reify an arbitary value */
      private def reify(value: Any): Tree = {
        value match {
          case tree: Tree =>
            reifyTree(tree)
          case sym: Symbol =>
            reifySymRef(sym)
          case tpe: Type =>
            reifyType(tpe)
          case xs: List[_] =>
            scalaFactoryCall("collection.immutable.List", xs map reify: _*)
          case xs: Array[_] =>
            scalaFactoryCall("Array", xs map reify: _*)
          case scope: Scope =>
            reifyScope(scope)
          case x: Name =>
            reifyName(x)
          case pos: Position => // todo: consider whether we should also reify positions
            reifyCaseObject(NoPosition)
          case Constant(_) | AnnotationInfo(_, _, _) | Modifiers(_, _, _) =>
            reifyCaseClassInstance(value.asInstanceOf[Product])
          case arg: ClassfileAnnotArg =>
            reifyCaseClassInstance(arg.asInstanceOf[Product])
          case x: Product if x.getClass.getName startsWith "scala.Tuple" =>
            reifyCaseClassInstance(x)
          case () => Literal(Constant(()))
          case x: String => Literal(Constant(x))
          case x: Boolean => Literal(Constant(x))
          case x: Byte => Literal(Constant(x))
          case x: Short => Literal(Constant(x))
          case x: Char => Literal(Constant(x))
          case x: Int => Literal(Constant(x))
          case x: Long => Literal(Constant(x))
          case x: Float => Literal(Constant(x))
          case x: Double => Literal(Constant(x))
          case _ => cannotReify(value)
        }
      }

      /** An (unreified) path that refers to definition with given fully qualified name
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
        ValDef(NoMods, mirrorShortName, TypeTree(), termPath(mirrorFullName))

      /** Generate code that generates a symbol table of all symbols registered in `reifiableSyms`
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

      private def cannotReify(value: Any): Nothing =
        abort("don't know how to reify " + value + " of class " + value.getClass)

    }

    def codify(tree: Tree): Tree = debugTrace("codified " + tree + " -> ") {
      val targetType = definitions.CodeClass.primaryConstructor.info.paramTypes.head
      val reifier = new Reifier()
      val arg = gen.mkAsInstanceOf(reifier.reifyTopLevel(tree), targetType, wrapInApply = false)
      New(TypeTree(appliedType(definitions.CodeClass.typeConstructor, List(tree.tpe))),
        List(List(arg)))
    }

    /**
     * PP: There is apparently some degree of overlap between the CAPTURED
     *  flag and the role being filled here.  I think this is how this was able
     *  to go for so long looking only at DefDef and Ident nodes, as bugs
     *  would only emerge under more complicated conditions such as #3855.
     *  I'll try to figure it all out, but if someone who already knows the
     *  whole story wants to fill it in, that too would be great.
     *
     *  XXX I found this had been cut and pasted between LiftCode and UnCurry,
     *  and seems to be running in both.
     */
    private val freeLocalsTraverser = new Traverser {
      var currentMethod: Symbol = NoSymbol
      var maybeEscaping = false

      def withEscaping(body: => Unit) {
        val saved = maybeEscaping
        maybeEscaping = true
        try body
        finally maybeEscaping = saved
      }

      override def traverse(tree: Tree) = tree match {
        case DefDef(_, _, _, _, _, _) =>
          val lastMethod = currentMethod
          currentMethod = tree.symbol
          try super.traverse(tree)
          finally currentMethod = lastMethod
        /** A method call with a by-name parameter represents escape. */
        case Apply(fn, args) if fn.symbol.paramss.nonEmpty =>
          traverse(fn)
          for ((param, arg) <- treeInfo.zipMethodParamsAndArgs(tree)) {
            if (param.tpe != null && isByNameParamType(param.tpe))
              withEscaping(traverse(arg))
            else
              traverse(arg)
          }

        /** The rhs of a closure represents escape. */
        case Function(vparams, body) =>
          vparams foreach traverse
          withEscaping(traverse(body))

        /**
         * The appearance of an ident outside the method where it was defined or
         *  anytime maybeEscaping is true implies escape.
         */
        case Ident(_) =>
          val sym = tree.symbol
          if (sym.isVariable && sym.owner.isMethod && (maybeEscaping || sym.owner != currentMethod)) {
            freeMutableVars += sym
            val symTpe = sym.tpe
            val symClass = symTpe.typeSymbol
            atPhase(phase.next) {
              def refType(valueRef: Map[Symbol, Symbol], objectRefClass: Symbol) =
                if (isValueClass(symClass)) valueRef(symClass).tpe
                else appliedType(objectRefClass.typeConstructor, List(symTpe))

              sym updateInfo (
                if (sym.hasAnnotation(VolatileAttr)) refType(volatileRefClass, VolatileObjectRefClass)
                else refType(refClass, ObjectRefClass))
            }
          }
        case _ =>
          super.traverse(tree)
      }
    }
  }
}
