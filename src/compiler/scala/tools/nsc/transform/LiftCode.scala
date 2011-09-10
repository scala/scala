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
 *  to the lifted "reflect trees" representation of exp.
 *  Also: mutable variables that are accessed from a local function are wrapped in refs.
 *
 *  @author Gilles Dubochet
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
    new Lifter(unit)

  class Lifter(unit: CompilationUnit) extends TypingTransformer(unit) {

    /** Set of mutable local variables that are free in some inner method. */
    private val freeMutableVars: mutable.Set[Symbol] = new mutable.HashSet
    private val converted: mutable.Set[Symbol] = new mutable.HashSet // debug

    override def transformUnit(unit: CompilationUnit) {
      freeMutableVars.clear()
      freeLocalsTraverser(unit.body)
      atPhase(phase.next) {
        super.transformUnit(unit)
      }
      for (v <- freeMutableVars)
        assert(converted contains v, "unconverted: "+v+" in "+v.owner+" in unit "+unit)
    }

    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
        case Apply(_, List(tree)) if sym == Code_lift => // reify Code.lift[T](expr) instances
          printTypings = true //debug
          val result = transform(localTyper.typedPos(tree.pos)(codify(tree)))
          println("transformed = "+result) //debug
          printTypings = false //debug
          result
        case ValDef(mods, name, tpt, rhs) if (freeMutableVars(sym)) => // box mutable variables that are accessed from a local closure
          val tpt1 = TypeTree(sym.tpe) setPos tpt.pos
          /* Creating a constructor argument if one isn't present. */
          val constructorArg = rhs match {
            case EmptyTree => gen.mkZero(atPhase(phase.prev)(sym.tpe))
            case _ => transform(rhs)
          }
          val rhs1 = typer.typedPos(rhs.pos) {
            /*util.errtrace("lifted rhs for "+tree+" in "+unit)*/ (
            Apply(Select(New(TypeTree(sym.tpe)), nme.CONSTRUCTOR), List(constructorArg)))
          }
          sym resetFlag MUTABLE
          sym removeAnnotation VolatileAttr
          converted += sym// dereference boxed variables
          treeCopy.ValDef(tree, mods &~ MUTABLE, name, tpt1, rhs1)
        case Ident(name) if freeMutableVars(sym) =>
          localTyper.typedPos(tree.pos) {
            /*util.errtrace("lifting ")*/(Select(tree setType sym.tpe, nme.elem))
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

      private val localSyms = mutable.ArrayBuffer[Symbol]()
      private val symIndex = mutable.HashMap[Symbol, Int]()
      private val typeTree = mutable.HashMap[Type, Tree]()
      private val typeTreeCount = mutable.HashMap[Tree, Int]()

      /** Generate tree of the form
       *
       *    { val $localSyms = Array(sym1, ..., symN)
       *      localSyms(1).setInfo(tpe1)
       *      ...
       *      localSyms(N).setInfo(tpeN)
       *      rtree
       *    }
       *
       *  where
       *
       *   - `symI` are the symbols defined locally in `tree`
       *   - `tpeI` are the info's of `symI`
       *   - `rtree` is code that generates `tree` at runtime, maintaining all attributes.
       */
      def reifyTopLevel(tree: Tree): Tree = {
        val rtree = reify(tree)
        memoize.transform(Block(mirrorAlias :: memoizerDef :: reifySymbolTableSetup, rtree))
      }

      private def registerLocalSymbol(sym: Symbol): Unit = {
        sym.owner.ownersIterator.find(!isLocatable(_)) match {
          case Some(outer) => registerLocalSymbol(outer)
          case None =>
        }
        symIndex(sym) = localSyms.length
        localSyms += sym
      }

      // helper methods

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

      private def isLocatable(sym: Symbol) =
        sym.isPackageClass || sym.owner.isClass || sym.isParameter && sym.paramPos >= 0

      /** Reify a reference to a symbol
       */
      private def reifySymRef(sym: Symbol): Tree = {
        symIndex get sym match {
          case Some(idx) =>
            Ident(localPrefix + symIndex(sym))
          case None =>
            if (sym.isStatic)
              mirrorCall(if (sym.isType) "staticClass" else "staticModule", reify(sym.fullName))
            else if (isLocatable(sym))
              if (sym.isParameter)
                mirrorCall("selectParam", reify(sym.owner), reify(sym.paramPos))
              else {
                val rowner = reify(sym.owner)
                val rname = reify(sym.name.toString)
                if (sym.isType) mirrorCall("selectType", rowner, rname)
                else mirrorCall("selectTerm", rowner, rname, reify(sym.tpe))
              }
            else {
              println("Late local: "+sym)
              assert(!sym.isParameter, sym+"/"+sym.owner+"/"+sym.owner.info+"/"+sym.paramPos)
              registerLocalSymbol(sym)
              reifySymRef(sym)
            }
        }
      }

      /** reify the creation of a symbol
       */
      private def reifySymbolDef(sym: Symbol): Tree = {
        println("reify sym def "+sym)
        var rsym: Tree = New(
            typePath(mirrorPrefix + (if (sym.isType) "TypeSymbol" else "TermSymbol")),
            List(List(reify(sym.owner), reify(sym.pos), reify(sym.name))))
        if (sym.flags != 0L)
          rsym = Apply(Select(rsym, "setFlag"), List(Literal(Constant(sym.flags))))
        ValDef(NoMods, localPrefix + symIndex(sym), TypeTree(), rsym)
      }

      /** Generate code to add type and annotation info to a reified symbol
       */
      private def fillInSymbol(sym: Symbol): Tree = {
        val rset = Apply(Select(reifySymRef(sym), "setInfo"), List(reifyType(sym.info)))
        if (sym.annotations.nonEmpty) rset
        else Apply(Select(rset, "setAnnotations"), List(reify(sym.annotations)))
      }

      /** Reify a scope */
      private def reifyScope(scope: Scope): Tree = {
        scope foreach registerLocalSymbol
        mirrorCall("newScopeWith", scope.toList map reifySymRef: _*)
      }

      /** Reify a list of symbols that need to be created */
      private def reifySymbols(syms: List[Symbol]): Tree = {
        syms foreach registerLocalSymbol
        mkList(syms map reifySymRef)
      }

      /** Reify a type that defines some symbols */
      private def reifyTypeBinder(value: Product, bound: List[Symbol], underlying: Type): Tree =
        mirrorFactoryCall(value, reifySymbols(bound), reify(underlying))

      /** Reify a type */
      private def reifyType(tpe: Type): Tree = typeTree get tpe match {
        case Some(tree) =>
          typeTreeCount(tree) += 1
          tree
        case None =>
          val tree = tpe match {
            case NoType | NoPrefix =>
              reifyCaseObject(tpe.asInstanceOf[Product])
            case ThisType(_) | SuperType(_, _) | SingleType(_, _) | ConstantType(_) |
              TypeRef(_, _, _) | AnnotatedType(_, _, _) |
              TypeBounds(_, _) | NullaryMethodType(_) | OverloadedType(_, _) =>
              reifyCaseClassInstance(tpe.asInstanceOf[Product])
            case t @ RefinedType(parents, decls) =>
              registerLocalSymbol(tpe.typeSymbol)
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
        typeTree(tpe) = tree
        typeTreeCount(tree) = 1
        tree
      }

      /** Reify a tree */
      private def reifyTree(tree: Tree): Tree = tree match {
        case tree @ This(_) if !(symIndex isDefinedAt tree.symbol) =>
          reifyFree(tree)
        case tree @ Ident(_) if !(symIndex isDefinedAt tree.symbol) =>
          reifyFree(tree)
        case _ =>
          var rtree = reifyCaseClassInstance(tree.asInstanceOf[Product])
          if (tree.isDef || tree.isInstanceOf[Function])
            registerLocalSymbol(tree.symbol)
          if (tree.hasSymbol)
            rtree = Apply(Select(rtree, "setSymbol"), List(reifySymRef(tree.symbol)))
          Apply(Select(rtree, "setType"), List(reifyType(tree.tpe)))
      }

      /** Reify a free reference. The result will be either a mirror reference
       *  to a global value, or else a mirror Literal.
       */
      private def reifyFree(tree: Tree): Tree =
        if (tree.symbol.hasFlag(MODULE) && tree.symbol.isStatic)
          reify(termPath(tree.symbol.fullName))
        else
          mirrorCall("Literal", tree)

      /** Reify an arbitary value */
      private def reify(value: Any): Tree = {
        //println("reifing "+value) //debug
        /*util.trace("reified "+value+" --> ")*/ {
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
            case pos: Position =>
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

      private def memoizerDef =
        ValDef(NoMods, memoizerName, TypeTree(), New(typePath("scala.reflect.runtime.Memoizer"), List(List())))

      /** Generate code that generates a symbol table of all symbols registered in `localSyms`
       */
      private def reifySymbolTableSetup: List[Tree] = {
        val symDefs, fillIns = new mutable.ArrayBuffer[Tree]
        var i = 0
        while (i < localSyms.length) {
          // fillInSymbol might create new localSyms, that's why this is done iteratively
          symDefs += reifySymbolDef(localSyms(i))
          fillIns += fillInSymbol(localSyms(i))
          i += 1
        }

        symDefs.toList ++ fillIns.toList
      }

      private object memoize extends Transformer {
        var memoCount = 0
        override def transform(tree: Tree) = typeTreeCount get tree match {
          case None => tree
          case Some(n) =>
            if (n > 0) {
              typeTreeCount(tree) = -memoCount
              val result = call(memoizerName+".add", reify(memoCount), tree)
              memoCount += 1
              result
            } else
              call(memoizerName+".get", reify(n))
        }
      }

      private def cannotReify(value: Any): Nothing =
        abort("don't know how to reify " + value + " of class " + value.getClass)

    }

    def codify(tree: Tree): Tree = util.trace("codified " + tree + " -> ") {
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

// case EmptyTree =>
// case LiftPoint(tree) =>
// case PackageDef(pid, stats) =>
// case ClassDef(mods, name, tparams, impl) =>
// case ValDef(mods, name, tpt, rhs) =>
// case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
// case TypeDef(mods, name, tparams, rhs) =>
// case LabelDef(name, params, rhs) =>
// case Template(parents, self, body) =>
// case Block(stats, expr) =>
// case ArrayValue(elemtpt, trees) =>
// case Assign(lhs, rhs) =>
// case If(cond, thenp, elsep) =>
// case Match(selector, cases) =>
// case Return(expr) =>
// case Try(block, catches, finalizer) =>
// case Throw(expr) =>
// case New(tpt) =>
// case Typed(expr, tpt) =>
// case TypeApply(fun, args) =>
// case Apply(fun, args) =>
// case Super(qual, mix) =>
// case This(qual) =>
// case Select(qualifier, selector) =>
// case Ident(name) =>
// case Literal(value) =>
// case TypeTree() =>
// /* Pattern matching */
// case CaseDef(pat, guard, body) =>
// case Alternative(trees) =>
// case Star(elem) =>
// case Bind(name, body) =>
