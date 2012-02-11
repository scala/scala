/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */
package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.collection.{ mutable, immutable }
import scala.tools.util.StringOps.{ ojoin }

/** Logic related to method synthesis which involves cooperation between
 *  Namer and Typer.
 */
trait MethodSynthesis {
  self: Analyzer =>

  import global._
  import definitions._
  import CODE._
  
  object synthesisUtil {
    type M[T]  = Manifest[T]
    type CM[T] = ClassManifest[T]

    def ValOrDefDef(sym: Symbol, body: Tree) =
      if (sym.isLazy) ValDef(sym, body)
      else DefDef(sym, body)

    def applyTypeInternal(manifests: List[M[_]]): Type = {
      val symbols = manifests map manifestToSymbol
      val container :: args = symbols
      val tparams = container.typeConstructor.typeParams

      // Conservative at present - if manifests were more usable this could do a lot more.
      require(symbols forall (_ ne NoSymbol), "Must find all manifests: " + symbols)
      require(container.owner.isPackageClass, "Container must be a top-level class in a package: " + container)
      require(tparams.size == args.size, "Arguments must match type constructor arity: " + tparams + ", " + args)

      typeRef(container.typeConstructor.prefix, container, args map (_.tpe))
    }
    
    def companionType[T](implicit m: M[T]) =
      getRequiredModule(m.erasure.getName).tpe

    // Use these like `applyType[List, Int]` or `applyType[Map, Int, String]`
    def applyType[CC](implicit m1: M[CC]): Type =
      applyTypeInternal(List(m1))

    def applyType[CC[X1], X1](implicit m1: M[CC[_]], m2: M[X1]): Type =
      applyTypeInternal(List(m1, m2))

    def applyType[CC[X1, X2], X1, X2](implicit m1: M[CC[_,_]], m2: M[X1], m3: M[X2]): Type =
      applyTypeInternal(List(m1, m2, m3))

    def applyType[CC[X1, X2, X3], X1, X2, X3](implicit m1: M[CC[_,_,_]], m2: M[X1], m3: M[X2], m4: M[X3]): Type =
      applyTypeInternal(List(m1, m2, m3, m4))

    def newMethodType[F](owner: Symbol)(implicit m: Manifest[F]): Type = {
      val fnSymbol = manifestToSymbol(m)
      assert(fnSymbol isSubClass FunctionClass(m.typeArguments.size - 1), (owner, m))
      val symbols = m.typeArguments map (m => manifestToSymbol(m))
      val formals = symbols.init map (_.typeConstructor)
      val params  = owner newSyntheticValueParams formals

      MethodType(params, symbols.last.typeConstructor)
    }
  }
  import synthesisUtil._

  class ClassMethodSynthesis(val clazz: Symbol, localTyper: Typer) {
    private def isOverride(name: TermName) =
      clazzMember(name).alternatives exists (sym => !sym.isDeferred && (sym.owner != clazz))
    
    def newMethodFlags(name: TermName) = {
      val overrideFlag = if (isOverride(name)) OVERRIDE else 0L
      overrideFlag | SYNTHETIC
    }
    def newMethodFlags(method: Symbol) = {
      val overrideFlag = if (isOverride(method.name)) OVERRIDE else 0L
      (method.flags | overrideFlag | SYNTHETIC) & ~DEFERRED
    }

    private def finishMethod(method: Symbol, f: Symbol => Tree): Tree =
      logResult("finishMethod")(localTyper typed ValOrDefDef(method, f(method)))

    private def createInternal(name: Name, f: Symbol => Tree, info: Type): Tree = {
      val m = clazz.newMethod(name.toTermName, clazz.pos.focus, newMethodFlags(name))
      finishMethod(m setInfoAndEnter info, f)
    }
    private def createInternal(name: Name, f: Symbol => Tree, infoFn: Symbol => Type): Tree = {
      val m = clazz.newMethod(name.toTermName, clazz.pos.focus, newMethodFlags(name))
      finishMethod(m setInfoAndEnter infoFn(m), f)
    }
    private def cloneInternal(original: Symbol, f: Symbol => Tree, name: Name): Tree = {
      val m = original.cloneSymbol(clazz, newMethodFlags(original)) setPos clazz.pos.focus
      m.name = name
      finishMethod(clazz.info.decls enter m, f)
    }

    private def cloneInternal(original: Symbol, f: Symbol => Tree): Tree =
      cloneInternal(original, f, original.name)

    def clazzMember(name: Name)  = clazz.info nonPrivateMember name
    def typeInClazz(sym: Symbol) = clazz.thisType memberType sym

    /** Function argument takes the newly created method symbol of
     *  the same type as `name` in clazz, and returns the tree to be
     *  added to the template.
     */
    def overrideMethod(name: Name)(f: Symbol => Tree): Tree =
      overrideMethod(clazzMember(name))(f)

    def overrideMethod(original: Symbol)(f: Symbol => Tree): Tree =
      cloneInternal(original, sym => f(sym setFlag OVERRIDE))

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

    def createSwitchMethod(name: Name, range: Seq[Int], returnType: Type)(f: Int => Tree) = {
      createMethod(name, List(IntClass.tpe), returnType) { m =>
        val arg0    = Ident(m.firstParam)
        val default = DEFAULT ==> THROW(IndexOutOfBoundsExceptionClass, arg0)
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
   *   1) enterGetterSetter is called from Namer with a ValDef which
   *   may need accessors.  Some setup is performed.  In general this
   *   creates symbols and enters them into the scope of the owner.
   *
   *   2) finishGetterSetter is called from Typer when a Template is typed.
   *   It completes the job, returning a list of trees with their symbols
   *   set to those created in enterGetterSetter.  Those trees then become
   *   part of the typed template.
   */
  trait MethodSynth {
    self: Namer =>

    import NamerErrorGen._

    def enterGetterSetter(tree: ValDef) {
      val ValDef(mods, name, _, _) = tree
      if (nme.isSetterName(name))
        ValOrValWithSetterSuffixError(tree)

      val getter = Getter(tree).createAndEnterSymbol()

      tree.symbol = (
        if (mods.isLazy) enterLazyVal(tree, getter)
        else {
          if (mods.isPrivateLocal)
            PrivateThisCaseClassParameterError(tree)
          // Create the setter if necessary.
          if (mods.isMutable)
            Setter(tree).createAndEnterSymbol()

          // If abstract, the tree gets the getter's symbol.  Otherwise, create a field.
          if (mods.isDeferred) getter setPos tree.pos
          else enterStrictVal(tree)
        }
      )

      enterBeans(tree)
    }
    def finishGetterSetter(typer: Typer, stat: Tree): List[Tree] = stat match {
      case vd @ ValDef(mods, name, tpt, rhs) if !noFinishGetterSetter(vd) =>
        // If we don't save the annotations, they seem to wander off.
        val annotations = stat.symbol.initialize.annotations
        val trees = (
          allValDefDerived(vd)
                  map (acc => atPos(vd.pos.focus)(acc derive annotations))
            filterNot (_ eq EmptyTree)
        )
        log(trees.mkString("Accessor trees:\n  ", "\n  ", "\n"))
        if (vd.symbol.isLazy) List(stat)
        else trees
      case _ =>
        List(stat)
    }

    def standardAccessors(vd: ValDef): List[DerivedFromValDef] = (
      if (vd.mods.isMutable && !vd.mods.isLazy) List(Getter(vd), Setter(vd))
      else List(Getter(vd))
    )
    def beanAccessors(vd: ValDef): List[DerivedFromValDef] = {
      if (forMSIL) Nil
      else if (vd.symbol hasAnnotation BeanPropertyAttr) {
        if (vd.mods.isMutable) List(BeanGetter(vd), BeanSetter(vd))
        else List(BeanGetter(vd))
      }
      else if (vd.symbol hasAnnotation BooleanBeanPropertyAttr)
        List(BooleanBeanGetter(vd))
      else Nil
    }
    def allValDefDerived(vd: ValDef) = {
      val field = if (vd.mods.isDeferred) Nil else List(Field(vd))
      field ::: standardAccessors(vd) ::: beanAccessors(vd)
    }

    trait Derived {
      def name: TermName
      def flagsMask: Long
      def flagsExtra: Long
      def completer(sym: Symbol): Type
    }
    trait DerivedFromValDef extends Derived {
      /** The declaration from which we are deriving.
       */
      def tree: ValDef

      /** Which meta-annotation is associated with this kind of entity.
       *  Presently one of: field, getter, setter, beanGetter, beanSetter, param.
       */
      def category: Symbol

      // Final methods to make the rest easier to reason about.
      final def mods      = tree.mods
      final def basisSym  = tree.symbol
      final def enclClass = basisSym.enclClass

      final def completer(sym: Symbol) = namerOf(sym).accessorTypeCompleter(tree, isSetter)
      final def fieldSelection         = Select(This(enclClass), basisSym)
      final def derivedFlags: Long     = basisSym.flags & flagsMask | flagsExtra
      final def derivedMods: Modifiers = mods & flagsMask | flagsExtra mapAnnotations (_ => Nil)

      def derivedSym: Symbol = tree.symbol
      def derivedTree: Tree  = EmptyTree

      def isSetter   = false
      def isDeferred = mods.isDeferred
      def keepClean  = false  // whether annotations whose definitions are not meta-annotated should be kept.
      def validate() { }
      def createAndEnterSymbol(): Symbol = {
        val sym = owner.newMethod(name, tree.pos.focus, (tree.mods.flags & flagsMask) | flagsExtra)
        setPrivateWithin(tree, sym)
        enterInScope(sym)
        sym setInfo completer(sym)
      }
      /** The annotations amongst those found on the original symbol which
       *  should be propagated to this kind of accessor.
       */
      private def deriveAnnotations(initial: List[AnnotationInfo]): List[AnnotationInfo] = {
        initial filter { ann =>
          // There are no meta-annotation arguments attached to `ann`
          if (ann.metaAnnotations.isEmpty) {
            // A meta-annotation matching `annotKind` exists on `ann`'s definition.
            (ann.defaultTargets contains category) ||
            // `ann`'s definition has no meta-annotations, and `keepClean` is true.
            (ann.defaultTargets.isEmpty && keepClean)
          }
          // There are meta-annotation arguments, and one of them matches `annotKind`
          else ann.metaAnnotations exists (_ matches category)
        }
      }
      private def logDerived(result: Tree): Tree = {
        log("[+derived] " + ojoin(mods.defaultFlagString, basisSym.accurateKindString, basisSym.getterName.decode)
          + " (" + derivedSym + ")\n        " + result)

        result
      }
      final def derive(initial: List[AnnotationInfo]): Tree = {
        validate()
        derivedSym setAnnotations deriveAnnotations(initial)
        logDerived(derivedTree)
      }
    }
    trait DerivedGetter extends DerivedFromValDef {
      // TODO
    }
    trait DerivedSetter extends DerivedFromValDef {
      override def isSetter = true
      private def setterParam = derivedSym.paramss match {
        case (p :: Nil) :: _  => p
        case _                => NoSymbol
      }
      private def setterRhs = (
        if (mods.isDeferred || derivedSym.isOverloaded) EmptyTree
        else Assign(fieldSelection, Ident(setterParam))
      )
      private def setterDef = DefDef(derivedSym, setterRhs)
      override def derivedTree: Tree = if (setterParam == NoSymbol) EmptyTree else setterDef
    }
    case class Getter(tree: ValDef) extends DerivedGetter {
      def name       = tree.name
      def category   = GetterTargetClass
      def flagsMask  = GetterFlags
      def flagsExtra = ACCESSOR | ( if (tree.mods.isMutable) 0 else STABLE )

      override def derivedSym = (
        if (mods.isDeferred) basisSym
        else basisSym.getter(enclClass)
      )
      override def validate() {
        assert(derivedSym != NoSymbol, tree)
        if (derivedSym.isOverloaded)
          GetterDefinedTwiceError(derivedSym)

        super.validate()
      }
      override def derivedTree: DefDef = {
        // For existentials, don't specify a type for the getter, even one derived
        // from the symbol! This leads to incompatible existentials for the field and
        // the getter. Let the typer do all the work. You might think "why only for
        // existentials, why not always," and you would be right, except: a single test
        // fails, but it looked like some work to deal with it. Test neg/t0606.scala
        // starts compiling (instead of failing like it's supposed to) because the typer
        // expects to be able to identify escaping locals in typedDefDef, and fails to
        // spot that brand of them. In other words it's an artifact of the implementation.
        val tpt = derivedSym.tpe.finalResultType match {
          case ExistentialType(_, _)  => TypeTree()
          case tp                     => TypeTree(tp)
        }
        tpt setPos focusPos(derivedSym.pos)
        // keep type tree of original abstract field
        if (mods.isDeferred)
          tpt setOriginal tree.tpt

        // TODO - reconcile this with the DefDef creator in Trees (which 
        //   at this writing presented no way to pass a tree in for tpt.)
        atPos(derivedSym.pos) {
          DefDef(
            Modifiers(derivedSym.flags),
            derivedSym.name.toTermName,
            Nil,
            Nil,
            tpt,
            if (mods.isDeferred) EmptyTree else gen.mkCheckInit(fieldSelection)
          ) setSymbol derivedSym
        }
      }
    }
    case class Setter(tree: ValDef) extends DerivedSetter {
      def name       = nme.getterToSetter(tree.name)
      def category   = SetterTargetClass
      def flagsMask  = SetterFlags
      def flagsExtra = ACCESSOR

      override def derivedSym = basisSym.setter(enclClass)
    }
    case class Field(tree: ValDef) extends DerivedFromValDef {
      def name       = nme.getterToLocal(tree.name)
      def category   = FieldTargetClass
      def flagsMask  = FieldFlags
      def flagsExtra = PrivateLocal
      // By default annotations go to the field, except if the field is
      // generated for a class parameter (PARAMACCESSOR).
      override def keepClean = !mods.isParamAccessor
      override def derivedTree = (
        if (mods.isDeferred) EmptyTree
        else treeCopy.ValDef(tree, mods | flagsExtra, name, tree.tpt, tree.rhs)
      )
    }
    case class Param(tree: ValDef) extends DerivedFromValDef {
      def name       = tree.name
      def category   = ParamTargetClass
      def flagsMask  = -1L
      def flagsExtra = 0L
      override def keepClean = true
      override def derivedTree = EmptyTree
    }
    def validateParam(tree: ValDef) {
      Param(tree).derive(tree.symbol.annotations)
    }

    sealed abstract class BeanAccessor(bean: String) extends DerivedFromValDef {
      val name       = newTermName(bean + tree.name.toString.capitalize)
      def flagsMask  = BeanPropertyFlags
      def flagsExtra = 0
      override def derivedSym = enclClass.info decl name
    }
    trait AnyBeanGetter extends BeanAccessor with DerivedGetter {
      def category = BeanGetterTargetClass
      override def validate() {
        if (derivedSym == NoSymbol) {
          // the namer decides whether to generate these symbols or not. at that point, we don't
          // have symbolic information yet, so we only look for annotations named "BeanProperty".
          BeanPropertyAnnotationLimitationError(tree)
        }
        super.validate()
      }
    }
    trait NoSymbolBeanGetter extends AnyBeanGetter {
      // Derives a tree without attempting to use the original tree's symbol.
      override def derivedTree = {
        atPos(tree.pos.focus) {
          DefDef(derivedMods, name, Nil, List(Nil), tree.tpt.duplicate,
            if (isDeferred) EmptyTree else Select(This(owner), tree.name)
          )
        }
      }
      override def createAndEnterSymbol(): Symbol = enterSyntheticSym(derivedTree)
    }
    case class BooleanBeanGetter(tree: ValDef) extends BeanAccessor("is") with AnyBeanGetter { }
    case class BeanGetter(tree: ValDef) extends BeanAccessor("get") with AnyBeanGetter { }
    case class BeanSetter(tree: ValDef) extends BeanAccessor("set") with DerivedSetter {
      def category = BeanSetterTargetClass
    }

    // No Symbols available.
    private def beanAccessorsFromNames(tree: ValDef) = {
      val ValDef(mods, name, tpt, _) = tree
      val hasBP     = mods hasAnnotationNamed tpnme.BeanPropertyAnnot
      val hasBoolBP = mods hasAnnotationNamed tpnme.BooleanBeanPropertyAnnot

      if (hasBP || hasBoolBP) {
        val getter = (
          if (hasBP) new BeanGetter(tree) with NoSymbolBeanGetter
          else new BooleanBeanGetter(tree) with NoSymbolBeanGetter
        )
        getter :: {
          if (mods.isMutable) List(BeanSetter(tree)) else Nil
        }
      }
      else Nil
    }

    protected def enterBeans(tree: ValDef) {
      if (forMSIL)
        return

      val ValDef(mods, name, _, _) = tree
      val beans = beanAccessorsFromNames(tree)
      if (beans.nonEmpty) {
        if (!name(0).isLetter)
          BeanPropertyAnnotationFieldWithoutLetterError(tree)
        else if (mods.isPrivate)  // avoids name clashes with private fields in traits
          BeanPropertyAnnotationPrivateFieldError(tree)

        // Create and enter the symbols here, add the trees in finishGetterSetter.
        beans foreach (_.createAndEnterSymbol())
      }
    }
  }
}
