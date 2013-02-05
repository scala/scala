/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */
package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.collection.{ mutable, immutable }
import scala.reflect.internal.util.StringOps.{ ojoin }
import scala.reflect.ClassTag
import scala.reflect.runtime.{ universe => ru }
import scala.language.higherKinds

/** Logic related to method synthesis which involves cooperation between
 *  Namer and Typer.
 */
trait MethodSynthesis {
  self: Analyzer =>

  import global._
  import definitions._
  import CODE._

  object synthesisUtil {
    type TT[T]  = ru.TypeTag[T]
    type CT[T] = ClassTag[T]

    def ValOrDefDef(sym: Symbol, body: Tree) =
      if (sym.isLazy) ValDef(sym, body)
      else DefDef(sym, body)

    def applyTypeInternal(tags: List[TT[_]]): Type = {
      val symbols = tags map compilerSymbolFromTag
      val container :: args = symbols
      val tparams = container.typeConstructor.typeParams

      // Conservative at present - if manifests were more usable this could do a lot more.
      // [Eugene to Paul] all right, they are now. what do you have in mind?
      require(symbols forall (_ ne NoSymbol), "Must find all tags: " + symbols)
      require(container.owner.isPackageClass, "Container must be a top-level class in a package: " + container)
      require(tparams.size == args.size, "Arguments must match type constructor arity: " + tparams + ", " + args)

      appliedType(container, args map (_.tpe): _*)
    }

    def companionType[T](implicit ct: CT[T]) =
      rootMirror.getRequiredModule(ct.runtimeClass.getName).tpe

    // Use these like `applyType[List, Int]` or `applyType[Map, Int, String]`
    def applyType[CC](implicit t1: TT[CC]): Type =
      applyTypeInternal(List(t1))

    def applyType[CC[X1], X1](implicit t1: TT[CC[_]], t2: TT[X1]): Type =
      applyTypeInternal(List(t1, t2))

    def applyType[CC[X1, X2], X1, X2](implicit t1: TT[CC[_,_]], t2: TT[X1], t3: TT[X2]): Type =
      applyTypeInternal(List(t1, t2, t3))

    def applyType[CC[X1, X2, X3], X1, X2, X3](implicit t1: TT[CC[_,_,_]], t2: TT[X1], t3: TT[X2], t4: TT[X3]): Type =
      applyTypeInternal(List(t1, t2, t3, t4))

    def newMethodType[F](owner: Symbol)(implicit t: TT[F]): Type = {
      val fnSymbol = compilerSymbolFromTag(t)
      val formals = compilerTypeFromTag(t).typeArguments
      assert(fnSymbol isSubClass FunctionClass(formals.size - 1), (owner, t))
      val params  = owner newSyntheticValueParams formals
      MethodType(params, formals.last)
    }

      /** The annotations amongst those found on the original symbol which
       *  should be propagated to this kind of accessor.
       */
      def deriveAnnotations(initial: List[AnnotationInfo], category: Symbol, keepClean: Boolean): List[AnnotationInfo] = {
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
   }
  import synthesisUtil._

  class ClassMethodSynthesis(val clazz: Symbol, localTyper: Typer) {
    def mkThis = This(clazz) setPos clazz.pos.focus
    def mkThisSelect(sym: Symbol) = atPos(clazz.pos.focus)(Select(mkThis, sym))

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
      localTyper typed ValOrDefDef(method, f(method))

    private def createInternal(name: Name, f: Symbol => Tree, info: Type): Tree = {
      val m = clazz.newMethod(name.toTermName, clazz.pos.focus, newMethodFlags(name))
      finishMethod(m setInfoAndEnter info, f)
    }
    private def createInternal(name: Name, f: Symbol => Tree, infoFn: Symbol => Type): Tree = {
      val m = clazz.newMethod(name.toTermName, clazz.pos.focus, newMethodFlags(name))
      finishMethod(m setInfoAndEnter infoFn(m), f)
    }
    private def cloneInternal(original: Symbol, f: Symbol => Tree, name: Name): Tree = {
      val m = original.cloneSymbol(clazz, newMethodFlags(original), name) setPos clazz.pos.focus
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
   *   1) Enter methods such as enterGetterSetterare called
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

    def enterImplicitWrapper(tree: ClassDef) {
      ImplicitClassWrapper(tree).createAndEnterSymbol()
    }

    def enterGetterSetter(tree: ValDef) {
      val ValDef(mods, name, _, _) = tree
      if (nme.isSetterName(name))
        ValOrValWithSetterSuffixError(tree)

      tree.symbol = (
        if (mods.isLazy) {
          val lazyValGetter = LazyValGetter(tree).createAndEnterSymbol()
          enterLazyVal(tree, lazyValGetter)
        } else {
          if (mods.isPrivateLocal)
            PrivateThisCaseClassParameterError(tree)
          val getter = Getter(tree).createAndEnterSymbol()
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

    def addDerivedTrees(typer: Typer, stat: Tree): List[Tree] = stat match {
      case vd @ ValDef(mods, name, tpt, rhs) if !noFinishGetterSetter(vd) =>
        // If we don't save the annotations, they seem to wander off.
        val annotations = stat.symbol.initialize.annotations
        ( allValDefDerived(vd)
                map (acc => atPos(vd.pos.focus)(acc derive annotations))
          filterNot (_ eq EmptyTree)
        )
      case cd @ ClassDef(mods, _, _, _) if mods.isImplicit =>
        val annotations = stat.symbol.initialize.annotations
        // TODO: need to shuffle annotations between wrapper and class.
        val wrapper = ImplicitClassWrapper(cd)
        val meth = wrapper.derivedSym
        context.unit.synthetics get meth match {
          case Some(mdef) =>
            context.unit.synthetics -= meth
            meth setAnnotations deriveAnnotations(annotations, MethodTargetClass, keepClean = false)
            cd.symbol setAnnotations deriveAnnotations(annotations, ClassTargetClass, keepClean = true)
            List(cd, mdef)
          case _ =>
            // Shouldn't happen, but let's give ourselves a reasonable error when it does
            abort("No synthetics for " + meth + ": synthetics contains " + context.unit.synthetics.keys.mkString(", "))
        }
      case _ =>
        stat :: Nil
      }

    def standardAccessors(vd: ValDef): List[DerivedFromValDef] = (
      if (vd.mods.isMutable && !vd.mods.isLazy) List(Getter(vd), Setter(vd))
      else if (vd.mods.isLazy) List(LazyValGetter(vd))
      else List(Getter(vd))
    )
    def beanAccessors(vd: ValDef): List[DerivedFromValDef] = {
      val setter = if (vd.mods.isMutable) List(BeanSetter(vd)) else Nil
      if (forMSIL) Nil
      else if (vd.symbol hasAnnotation BeanPropertyAttr)
        BeanGetter(vd) :: setter
      else if (vd.symbol hasAnnotation BooleanBeanPropertyAttr)
        BooleanBeanGetter(vd) :: setter
      else Nil
    }
    def allValDefDerived(vd: ValDef) = {
      val field = if (vd.mods.isDeferred || (vd.mods.isLazy && hasUnitType(vd.symbol))) Nil
                  else List(Field(vd))
      field ::: standardAccessors(vd) ::: beanAccessors(vd)
    }

    // Take into account annotations so that we keep annotated unit lazy val
    // to get better error message already from the cps plugin itself
    def hasUnitType(sym: Symbol) = (sym.tpe.typeSymbol == UnitClass) && sym.tpe.annotations.isEmpty

    /** This trait assembles what's needed for synthesizing derived methods.
     *  Important: Typically, instances of this trait are created TWICE for each derived
     *  symbol; once form Namers in an enter method, and once from Typers in addDerivedTrees.
     *  So it's important that creating an instance of Derived does not have a side effect,
     *  or if it has a side effect, control that it is done only once.
     */
    trait Derived {

      /** The tree from which we are deriving a synthetic member. Typically, that's
       *  given as an argument of the instance. */
      def tree: Tree

      /** The name of the method */
      def name: TermName

      /** The flags that are retained from the original symbol */

      def flagsMask: Long

      /** The flags that the derived symbol has in addition to those retained from
       *  the original symbol*/
      def flagsExtra: Long

      /** type completer for the synthetic member.
       */
      def completer(sym: Symbol): Type

      /** The derived symbol. It is assumed that this symbol already exists and has been
       *  entered in the parent scope when derivedSym is called */
      def derivedSym: Symbol

      /** The definition tree of the derived symbol. */
      def derivedTree: Tree
    }

    trait DerivedFromMemberDef extends Derived {
      def tree: MemberDef
      def enclClass: Symbol

      // Final methods to make the rest easier to reason about.
      final def mods               = tree.mods
      final def basisSym           = tree.symbol
      final def derivedFlags: Long = basisSym.flags & flagsMask | flagsExtra
    }

    trait DerivedFromClassDef extends DerivedFromMemberDef {
      def tree: ClassDef
      final def enclClass = basisSym.owner.enclClass
    }

    trait DerivedFromValDef extends DerivedFromMemberDef {
      def tree: ValDef
      final def enclClass = basisSym.enclClass

      /** Which meta-annotation is associated with this kind of entity.
       *  Presently one of: field, getter, setter, beanGetter, beanSetter, param.
       */
      def category: Symbol

      /* Explicit isSetter required for bean setters (beanSetterSym.isSetter is false) */
      final def completer(sym: Symbol) = namerOf(sym).accessorTypeCompleter(tree, isSetter)
      final def fieldSelection         = Select(This(enclClass), basisSym)
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
      private def logDerived(result: Tree): Tree = {
        debuglog("[+derived] " + ojoin(mods.flagString, basisSym.accurateKindString, basisSym.getterName.decode)
          + " (" + derivedSym + ")\n        " + result)

        result
      }
      final def derive(initial: List[AnnotationInfo]): Tree = {
        validate()
        derivedSym setAnnotations deriveAnnotations(initial, category, keepClean)
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

    /** A synthetic method which performs the implicit conversion implied by
     *  the declaration of an implicit class.
     */
    case class ImplicitClassWrapper(tree: ClassDef) extends DerivedFromClassDef {
      def completer(sym: Symbol): Type = ??? // not needed
      def createAndEnterSymbol(): Symbol = enterSyntheticSym(derivedTree)
      def derivedSym: Symbol = {
        // Only methods will do! Don't want to pick up any stray
        // companion objects of the same name.
        val result = enclClass.info decl name suchThat (x => x.isMethod && x.isSynthetic)
        assert(result != NoSymbol, "not found: "+name+" in "+enclClass+" "+enclClass.info.decls)
        result
      }
      def derivedTree: DefDef          =
        factoryMeth(mods & flagsMask | flagsExtra, name, tree)
      def flagsExtra: Long             = METHOD | IMPLICIT | SYNTHETIC
      def flagsMask: Long              = AccessFlags
      def name: TermName               = tree.name.toTermName
    }

    abstract class BaseGetter(tree: ValDef) extends DerivedGetter {
      def name       = tree.name
      def category   = GetterTargetClass
      def flagsMask  = GetterFlags
      def flagsExtra = ACCESSOR | ( if (tree.mods.isMutable) 0 else STABLE )

      override def validate() {
        assert(derivedSym != NoSymbol, tree)
        if (derivedSym.isOverloaded)
          GetterDefinedTwiceError(derivedSym)

        super.validate()
      }
    }
    case class Getter(tree: ValDef) extends BaseGetter(tree) {
      override def derivedSym = (
        if (mods.isDeferred) basisSym
        else basisSym.getter(enclClass)
      )

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
          case _ if mods.isDeferred   => TypeTree()
          case tp                     => TypeTree(tp)
        }
        tpt setPos derivedSym.pos.focus
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
    /** Implements lazy value accessors:
     *    - for lazy values of type Unit and all lazy fields inside traits,
     *      the rhs is the initializer itself
     *    - for all other lazy values z the accessor is a block of this form:
     *      { z = <rhs>; z } where z can be an identifier or a field.
     */
    case class LazyValGetter(tree: ValDef) extends BaseGetter(tree) {
      class ChangeOwnerAndModuleClassTraverser(oldowner: Symbol, newowner: Symbol)
        extends ChangeOwnerTraverser(oldowner, newowner) {
        
        override def traverse(tree: Tree) {
          tree match {
            case _: DefTree => change(tree.symbol.moduleClass)
            case _          =>
          }
          super.traverse(tree)
        }
      }

      // todo: in future this should be enabled but now other phases still depend on the flag for various reasons
      //override def flagsMask = (super.flagsMask & ~LAZY)
      override def derivedSym = basisSym.lazyAccessor
      override def derivedTree: DefDef = {
        val ValDef(_, _, tpt0, rhs0) = tree
        val rhs1 = transformed.getOrElse(rhs0, rhs0)
        val body = (
          if (tree.symbol.owner.isTrait || hasUnitType(basisSym)) rhs1
          else gen.mkAssignAndReturn(basisSym, rhs1)
        )
        derivedSym.setPos(tree.pos) // cannot set it at createAndEnterSymbol because basisSym can possible stil have NoPosition
        val ddefRes = atPos(tree.pos)(DefDef(derivedSym, new ChangeOwnerAndModuleClassTraverser(basisSym, derivedSym)(body)))
        // ValDef will have its position focused whereas DefDef will have original correct rangepos
        // ideally positions would be correct at the creation time but lazy vals are really a special case
        // here so for the sake of keeping api clean we fix positions manually in LazyValGetter
        ddefRes.tpt.setPos(tpt0.pos)
        tpt0.setPos(tpt0.pos.focus)
        ddefRes
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
        else if (mods.isLazy) copyValDef(tree)(mods = mods | flagsExtra, name = this.name, rhs = EmptyTree).setPos(tree.pos.focus)
        else copyValDef(tree)(mods = mods | flagsExtra, name = this.name)
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
          DefDef(derivedMods, name, Nil, ListOfNil, tree.tpt.duplicate,
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
        if (!name.charAt(0).isLetter)
          BeanPropertyAnnotationFieldWithoutLetterError(tree)
        else if (mods.isPrivate)  // avoids name clashes with private fields in traits
          BeanPropertyAnnotationPrivateFieldError(tree)

        // Create and enter the symbols here, add the trees in finishGetterSetter.
        beans foreach (_.createAndEnterSymbol())
      }
    }
  }
}
