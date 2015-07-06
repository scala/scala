/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */
package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.reflect.internal.util.StringOps.{ ojoin }
import scala.reflect.ClassTag
import scala.reflect.internal.util.ListOfNil
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

  /** The annotations amongst those found on the original symbol which
   *  should be propagated to this kind of accessor.
   */
  def deriveAnnotations(initial: List[AnnotationInfo], category: Symbol, keepClean: Boolean): List[AnnotationInfo] = {
    def annotationFilter(ann: AnnotationInfo) = ann.metaAnnotations match {
      case Nil if ann.defaultTargets.isEmpty => keepClean                             // no meta-annotations or default targets
      case Nil                               => ann.defaultTargets contains category  // default targets exist for ann
      case metas                             => metas exists (_ matches category)     // meta-annotations attached to ann
    }
    initial filter annotationFilter
  }

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

    def createSwitchMethod(name: Name, range: Seq[Int], returnType: Type)(f: Int => Tree) = {
      createMethod(name, List(IntTpe), returnType) { m =>
        val arg0    = Ident(m.firstParam)
        val default = DEFAULT ==> Throw(IndexOutOfBoundsExceptionClass.tpe_*, fn(arg0, nme.toString_))
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

    /** This is called for those ValDefs which addDerivedTrees ignores, but
     *  which might have a warnable annotation situation.
     */
    private def warnForDroppedAnnotations(tree: Tree) {
      val annotations   = tree.symbol.initialize.annotations
      val targetClass   = defaultAnnotationTarget(tree)
      val retained      = deriveAnnotations(annotations, targetClass, keepClean = true)

      annotations filterNot (retained contains _) foreach (ann => issueAnnotationWarning(tree, ann, targetClass))
    }
    private def issueAnnotationWarning(tree: Tree, ann: AnnotationInfo, defaultTarget: Symbol) {
      global.reporter.warning(ann.pos,
        s"no valid targets for annotation on ${tree.symbol} - it is discarded unused. " +
        s"You may specify targets with meta-annotations, e.g. @($ann @${defaultTarget.name})")
    }

    def addDerivedTrees(typer: Typer, stat: Tree): List[Tree] = stat match {
      case vd @ ValDef(mods, name, tpt, rhs) if !noFinishGetterSetter(vd) =>
        // If we don't save the annotations, they seem to wander off.
        val annotations = stat.symbol.initialize.annotations
        val trees = (
          allValDefDerived(vd)
                map (acc => atPos(vd.pos.focus)(acc derive annotations))
          filterNot (_ eq EmptyTree)
        )
        // Verify each annotation landed safely somewhere, else warn.
        // Filtering when isParamAccessor is a necessary simplification
        // because there's a bunch of unwritten annotation code involving
        // the propagation of annotations - constructor parameter annotations
        // may need to make their way to parameters of the constructor as
        // well as fields of the class, etc.
        if (!mods.isParamAccessor) annotations foreach (ann =>
          if (!trees.exists(_.symbol hasAnnotation ann.symbol))
            issueAnnotationWarning(vd, ann, GetterTargetClass)
        )

        trees
      case vd: ValDef =>
        warnForDroppedAnnotations(vd)
        vd :: Nil
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
            context.error(cd.pos, s"Internal error: Symbol for synthetic factory method not found among ${context.unit.synthetics.keys.mkString(", ")}")
            // Soldier on for the sake of the presentation compiler
            List(cd)
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
      if (vd.symbol hasAnnotation BeanPropertyAttr)
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
    sealed trait Derived {

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

    sealed trait DerivedFromMemberDef extends Derived {
      def tree: MemberDef
      def enclClass: Symbol

      // Final methods to make the rest easier to reason about.
      final def mods               = tree.mods
      final def basisSym           = tree.symbol
    }

    sealed trait DerivedFromClassDef extends DerivedFromMemberDef {
      def tree: ClassDef
      final def enclClass = basisSym.owner.enclClass
    }

    sealed trait DerivedFromValDef extends DerivedFromMemberDef {
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
    sealed trait DerivedGetter extends DerivedFromValDef {
      // TODO
    }
    sealed trait DerivedSetter extends DerivedFromValDef {
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
        val result = enclClass.info decl name filter (x => x.isMethod && x.isSynthetic)
        if (result == NoSymbol || result.isOverloaded)
          context.error(tree.pos, s"Internal error: Unable to find the synthetic factory method corresponding to implicit class $name in $enclClass / ${enclClass.info.decls}")
        result
      }
      def derivedTree: DefDef          =
        factoryMeth(mods & flagsMask | flagsExtra, name, tree)
      def flagsExtra: Long             = METHOD | IMPLICIT | SYNTHETIC
      def flagsMask: Long              = AccessFlags
      def name: TermName               = tree.name.toTermName
    }

    sealed abstract class BaseGetter(tree: ValDef) extends DerivedGetter {
      def name       = tree.name
      def category   = GetterTargetClass
      def flagsMask  = GetterFlags
      def flagsExtra = ACCESSOR.toLong | ( if (tree.mods.isMutable) 0 else STABLE )

      override def validate() {
        assert(derivedSym != NoSymbol, tree)
        if (derivedSym.isOverloaded)
          GetterDefinedTwiceError(derivedSym)

        super.validate()
      }
    }
    case class Getter(tree: ValDef) extends BaseGetter(tree) {
      override def derivedSym = if (mods.isDeferred) basisSym else basisSym.getterIn(enclClass)
      private def derivedRhs  = if (mods.isDeferred) EmptyTree else fieldSelection
      private def derivedTpt = {
        // For existentials, don't specify a type for the getter, even one derived
        // from the symbol! This leads to incompatible existentials for the field and
        // the getter. Let the typer do all the work. You might think "why only for
        // existentials, why not always," and you would be right, except: a single test
        // fails, but it looked like some work to deal with it. Test neg/t0606.scala
        // starts compiling (instead of failing like it's supposed to) because the typer
        // expects to be able to identify escaping locals in typedDefDef, and fails to
        // spot that brand of them. In other words it's an artifact of the implementation.
        val tpt = derivedSym.tpe_*.finalResultType.widen match {
          // Range position errors ensue if we don't duplicate this in some
          // circumstances (at least: concrete vals with existential types.)
          case ExistentialType(_, _)  => TypeTree() setOriginal (tree.tpt.duplicate setPos tree.tpt.pos.focus)
          case _ if mods.isDeferred   => TypeTree() setOriginal tree.tpt // keep type tree of original abstract field
          case tp                     => TypeTree(tp)
        }
        tpt setPos tree.tpt.pos.focus
      }
      override def derivedTree: DefDef = newDefDef(derivedSym, derivedRhs)(tpt = derivedTpt)
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
        val rhs1 = context.unit.transformed.getOrElse(rhs0, rhs0)
        val body = (
          if (tree.symbol.owner.isTrait || hasUnitType(basisSym)) rhs1
          else gen.mkAssignAndReturn(basisSym, rhs1)
        )
        derivedSym setPos tree.pos // cannot set it at createAndEnterSymbol because basisSym can possibly still have NoPosition
        val ddefRes = DefDef(derivedSym, new ChangeOwnerAndModuleClassTraverser(basisSym, derivedSym)(body))
        // ValDef will have its position focused whereas DefDef will have original correct rangepos
        // ideally positions would be correct at the creation time but lazy vals are really a special case
        // here so for the sake of keeping api clean we fix positions manually in LazyValGetter
        ddefRes.tpt.setPos(tpt0.pos)
        tpt0.setPos(tpt0.pos.focus)
        ddefRes
      }
    }
    case class Setter(tree: ValDef) extends DerivedSetter {
      def name       = tree.setterName
      def category   = SetterTargetClass
      def flagsMask  = SetterFlags
      def flagsExtra = ACCESSOR

      override def derivedSym = basisSym.setterIn(enclClass)
    }
    case class Field(tree: ValDef) extends DerivedFromValDef {
      def name       = tree.localName
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
    sealed trait AnyBeanGetter extends BeanAccessor with DerivedGetter {
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
      val ValDef(mods, _, _, _) = tree
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
