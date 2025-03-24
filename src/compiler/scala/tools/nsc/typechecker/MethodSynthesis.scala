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
package typechecker

import scala.reflect.NameTransformer
import symtab.Flags._

/** Logic related to method synthesis which involves cooperation between
 *  Namer and Typer.
 */
trait MethodSynthesis {
  self: Analyzer =>

  import global._
  import definitions._
  import CODE._


  class ClassMethodSynthesis(val clazz: Symbol, localTyper: Typer) {
    def mkThis = This(clazz) setPos clazz.pos.focus
    def mkThisSelect(sym: Symbol) = atPos(clazz.pos.focus)(
      if (clazz.isClass) Select(This(clazz), sym) else Ident(sym)
    )

    private def isOverride(name: TermName) =
      clazzMember(name).alternatives exists (sym => !sym.isDeferred && (sym.owner != clazz))

    def newMethodFlags(name: TermName) = {
      val overrideFlag = if (isOverride(name)) OVERRIDE else 0L
      overrideFlag | SYNTHETIC
    }
    def newMethodFlags(method: Symbol) = {
      val overrideFlag = if (isOverride(method.name.toTermName)) OVERRIDE else 0L
      (method.flags | overrideFlag | SYNTHETIC) & ~DEFERRED
    }

    private def finishMethod(method: Symbol, f: Symbol => Tree): Tree =
      localTyper typed (
        if (method.isLazy) ValDef(method, f(method))
        else DefDef(method, f(method))
      )

    private def createInternal(name: Name, f: Symbol => Tree, info: Type): Tree = {
      val name1 = name.toTermName
      val m = clazz.newMethod(name1, clazz.pos.focus, newMethodFlags(name1))
      finishMethod(m setInfoAndEnter info, f)
    }
    private def createInternal(name: Name, f: Symbol => Tree, infoFn: Symbol => Type): Tree = {
      val name1 = name.toTermName
      val m = clazz.newMethod(name1, clazz.pos.focus, newMethodFlags(name1))
      finishMethod(m setInfoAndEnter infoFn(m), f)
    }
    private def cloneInternal(original: Symbol, f: Symbol => Tree, name: Name): Tree = {
      val m = original.cloneSymbol(clazz, newMethodFlags(original), name) setPos clazz.pos.focus
      finishMethod(clazz.info.decls enter m, f)
    }

    def clazzMember(name: Name)  = clazz.info nonPrivateMember name
    def typeInClazz(sym: Symbol) = clazz.thisType memberType sym

    def deriveMethod(original: Symbol, nameFn: Name => Name)(f: Symbol => Tree): Tree =
      cloneInternal(original, f, nameFn(original.name))

    def createMethod(name: Name, paramTypes: List[Type], returnType: Type)(f: Symbol => Tree): Tree =
      createInternal(name, f, (m: Symbol) => MethodType(m newSyntheticValueParams paramTypes, returnType))

    def createMethod(name: Name, returnType: Type)(f: Symbol => Tree): Tree =
      createInternal(name, f, NullaryMethodType(returnType))

    def createMethod(original: Symbol)(f: Symbol => Tree): Tree =
      createInternal(original.name, f, original.info)

    def forwardMethod(original: Symbol, newMethod: Symbol)(transformArgs: List[Tree] => List[Tree]): Tree =
      createMethod(original)(m => gen.mkMethodCall(newMethod, transformArgs(m.paramss.head map Ident)))

    def createSwitchMethod(name: Name, range: Seq[Int], returnType: Type)(f: Int => Tree): Tree = {
      def dflt(arg: Tree) = currentRun.runDefinitions.RuntimeStatics_ioobe match {
        case NoSymbol =>
          // Support running the compiler with an older library on the classpath
          Throw(IndexOutOfBoundsExceptionClass.tpe_*, fn(arg, nme.toString_))
        case ioobeSym =>
          val ioobeTypeApply = TypeApply(gen.mkAttributedRef(ioobeSym), List(TypeTree(returnType)))
          Apply(ioobeTypeApply, List(arg))
      }

      createMethod(name, List(IntTpe), returnType) { m =>
        val arg0    = Ident(m.firstParam)
        val default = DEFAULT ==> dflt(arg0)
        val cases   = range.map(num => CASE(LIT(num)) ==> f(num)).toList :+ default

        Match(arg0, cases)
      }
    }

    // def foo() = constant
    def constantMethod(name: Name, value: Any): Tree = {
      val constant = Constant(value)
      createMethod(name, Nil, constant.tpe)(_ => Literal(constant))
    }
    // def foo = constant
    def constantNullary(name: Name, value: Any): Tree = {
      val constant = Constant(value)
      createMethod(name, constant.tpe)(_ => Literal(constant))
    }
  }

  /** There are two key methods in here.
   *
   *   1) Enter methods such as enterGetterSetter are called
   *   from Namer with a tree which may generate further trees such as accessors or
   *   implicit wrappers. Some setup is performed.  In general this creates symbols
   *   and enters them into the scope of the owner.
   *
   *   2) addDerivedTrees is called from Typer when a Template is typed.
   *   It completes the job, returning a list of trees with their symbols
   *   set to those created in the enter methods.  Those trees then become
   *   part of the typed template.
   */
  trait MethodSynth {
    self: Namer =>

    import NamerErrorGen._


    import treeInfo.noFieldFor

    // populate synthetics for this unit with trees that will later be added by the typer
    // we get here when entering the symbol for the valdef, so its rhs has not yet been type checked
    def enterGetterSetter(tree: ValDef): Unit = {
      val sympos = tree.namePos
      val fieldSym =
        if (noFieldFor(tree, owner)) NoSymbol
        else owner.newValue(tree.name append NameTransformer.LOCAL_SUFFIX_STRING, sympos, tree.mods.flags & FieldFlags | PrivateLocal)

      val getter = Getter(tree)
      val getterSym = getter.createSym

      // only one symbol can have `tree.pos`, the others must focus their position
      // normally the field gets the range position, but if there is none, give it to the getter
      //
      // scala/bug#10009 the tree's modifiers can be temporarily out of sync with the new symbol's flags.
      //          typedValDef corrects this later on.
      tree.symbol = fieldSym orElse getterSym.setPos(sympos)

      val namer = namerOf(tree.symbol)

      // the valdef gets the accessor symbol for a lazy val (too much going on in its RHS)
      // the fields phase creates the field symbol
      if (!tree.mods.isLazy) {
        // if there's a field symbol, the getter is considered a synthetic that must be added later
        // if there's no field symbol, the ValDef tree receives the getter symbol and thus is not a synthetic
        if (fieldSym != NoSymbol) {
          context.unit.synthetics(getterSym) = getter.derivedTree(getterSym)
          getterSym setInfo namer.accessorTypeCompleter(tree, tree.tpt.isEmpty, isBean = false, isSetter = false)
        } else getterSym setInfo namer.valTypeCompleter(tree)

        enterInScope(getterSym)

        if (getter.needsSetter) {
          val setter = Setter(tree)
          val setterSym = setter.createSym
          context.unit.synthetics(setterSym) = setter.derivedTree(setterSym)
          setterSym setInfo namer.accessorTypeCompleter(tree, tree.tpt.isEmpty, isBean = false, isSetter = true)
          enterInScope(setterSym)
        }

        // TODO: delay emitting the field to the fields phase (except for private[this] vals, which only get a field and no accessors)
        if (fieldSym != NoSymbol) {
          fieldSym setInfo namer.valTypeCompleter(tree)
          enterInScope(fieldSym)
        }
      } else {
        getterSym setInfo namer.valTypeCompleter(tree)
        enterInScope(getterSym)
      }

      deriveBeanAccessors(tree, namer)
    }

    private def deriveBeanAccessors(tree: ValDef, namer: Namer): Unit = {
      // TODO: can we look at the annotations symbols? (name-based introduced in 8cc477f8b6, see neg/t3403)
      val hasBeanProperty = tree.mods hasAnnotationNamed tpnme.BeanPropertyAnnot
      val hasBoolBP = tree.mods hasAnnotationNamed tpnme.BooleanBeanPropertyAnnot

      if (hasBeanProperty || hasBoolBP) {
        if (!tree.name.charAt(0).isLetter) BeanPropertyAnnotationFieldWithoutLetterError(tree)
        // avoids name clashes with private fields in traits
        else if (tree.mods.isPrivate) BeanPropertyAnnotationPrivateFieldError(tree)

        val derivedPos = tree.pos.focus
        val missingTpt = tree.tpt.isEmpty

        def deriveBeanAccessor(prefix: String): Symbol = {
          val isSetter = prefix == "set"
          val name = newTermName(prefix + tree.name.toString.capitalize)
          val setterParam = nme.syntheticParamName(1)

          // note: tree.tpt may be EmptyTree, which will be a problem when use as the tpt of a parameter
          // the completer will patch this up (we can't do this now without completing the field)
          val tptToPatch = if (missingTpt) TypeTree() else tree.tpt.duplicate

          val (vparams, tpt) =
            if (isSetter) (List(ValDef(Modifiers(PARAM | SYNTHETIC), setterParam, tptToPatch, EmptyTree)), TypeTree(UnitTpe))
            else (Nil, tptToPatch)

          val rhs =
            if (tree.mods.isDeferred) EmptyTree
            else if (isSetter) Apply(Ident(tree.name.setterName), List(Ident(setterParam)))
            else Select(This(owner), tree.name)

          val sym = createMethod(tree, name, derivedPos, tree.mods.flags & BeanPropertyFlags)
          context.unit.synthetics(sym) = newDefDef(sym, rhs)(tparams = Nil, vparamss = List(vparams), tpt = tpt)
          sym
        }

        val getterCompleter = namer.accessorTypeCompleter(tree, missingTpt, isBean = true, isSetter = false)
        enterInScope(deriveBeanAccessor(if (hasBeanProperty) "get" else "is") setInfo getterCompleter)

        if (tree.mods.isMutable) {
          val setterCompleter = namer.accessorTypeCompleter(tree, missingTpt, isBean = true, isSetter = true)
          enterInScope(deriveBeanAccessor("set") setInfo setterCompleter)
        }
      }
    }

    def enterImplicitWrapper(classDef: ClassDef): Unit = {
      val methDef = factoryMeth(classDef.mods & (AccessFlags | FINAL) | METHOD | IMPLICIT | SYNTHETIC, classDef.name.toTermName, classDef)
      val methSym = enterInScope(assignMemberSymbol(methDef))
      context.unit.synthetics(methSym) = methDef

      treeInfo.firstConstructor(classDef.impl.body) match {
        case primaryConstructor: DefDef =>
          if (mexists(primaryConstructor.vparamss)(_.mods.hasDefault))
            enterDefaultGetters(methSym, primaryConstructor, primaryConstructor.vparamss, primaryConstructor.tparams)
        case _ =>
      }
      methSym setInfo implicitFactoryMethodCompleter(methDef, classDef.symbol)
    }

    trait DerivedAccessor {
      def tree: ValDef
      def derivedName: TermName
      def derivedFlags: Long
      def derivedTree(sym: Symbol): Tree
      def derivedPos = tree.pos.focus
      def symPos = tree.namePos
      def createSym = createMethod(tree, derivedName, symPos, derivedFlags)
    }

    case class Getter(tree: ValDef) extends DerivedAccessor {
      def derivedName  = tree.name
      def derivedFlags = tree.mods.flags & GetterFlags | ACCESSOR.toLong | ( if (needsSetter) 0 else STABLE )
      def needsSetter  = tree.mods.isMutable  // implies !lazy

      override def derivedTree(derivedSym: Symbol) = {
        val missingTpt = tree.tpt.isEmpty
        val tpt = if (missingTpt) TypeTree() else tree.tpt.duplicate

        val rhs =
          if (noFieldFor(tree, owner)) tree.rhs // context.unit.transformed.getOrElse(tree.rhs, tree.rhs)
          else Select(This(tree.symbol.enclClass), tree.symbol)

        newDefDefAt(derivedPos)(derivedSym, rhs)(tparams = Nil, vparamss = Nil, tpt = tpt)
      }

//        derivedSym setPos tree.pos
//        // ValDef will have its position focused whereas DefDef will have original correct rangepos
//        // ideally positions would be correct at the creation time but lazy vals are really a special case
//        // here so for the sake of keeping api clean we fix positions manually in LazyValGetter
//        tpt.setPos(tree.tpt.pos)
//        tree.tpt.setPos(tree.tpt.pos.focus)

    }

    case class Setter(tree: ValDef) extends DerivedAccessor {
      def derivedName  = tree.setterName
      def derivedFlags = tree.mods.flags & SetterFlags | ACCESSOR
      def derivedTree(derivedSym: Symbol)  = {
        val setterParam = nme.syntheticParamName(1)

        // note: tree.tpt may be EmptyTree, which will be a problem when use as the tpt of a parameter
        // the completer will patch this up (we can't do this now without completing the field)
        val missingTpt = tree.tpt.isEmpty
        val tptToPatch = if (missingTpt) TypeTree() else tree.tpt.duplicate

        val vparams = List(ValDef(Modifiers(PARAM | SYNTHETIC), setterParam, tptToPatch, EmptyTree))

        val tpt = TypeTree(UnitTpe)

        val rhs =
          if (noFieldFor(tree, owner)) EmptyTree
          else Assign(Select(This(tree.symbol.enclClass), tree.symbol), Ident(setterParam))

        newDefDefAt(derivedPos)(derivedSym, rhs)(tparams = Nil, vparamss = List(vparams), tpt = tpt)
      }
    }
  }
}
