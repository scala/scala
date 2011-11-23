/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */
package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.collection.{ mutable, immutable }

object listutil {
  def mexists[T](xss: List[List[T]])(p: T => Boolean) =
    xss exists (_ exists p)
  def mmap[T, U](xss: List[List[T]])(f: T => U) =
    xss map (_ map f)
  def mforeach[T](xss: List[List[T]])(f: T => Unit) =
    xss foreach (_ foreach f)
  def mfind[T](xss: List[List[T]])(p: T => Boolean): Option[T] = {
    for (xs <- xss; x <- xs)
      if (p(x)) return Some(x)
    None
  }
  def mfilter[T](xss: List[List[T]])(p: T => Boolean) =
    for (xs <- xss; x <- xs; if p(x)) yield x
}

/** Logic related to method synthesis which involves cooperation between
 *  Namer and Typer.
 */
trait MethodSynthesis {
  self: Analyzer =>

  import global._
  import definitions._

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

    def enterGetterSetter(tree: ValDef) {
      val ValDef(mods, name, _, _) = tree
      if (nme.isSetterName(name))
        context.error(tree.pos, "Names of vals or vars may not end in `_='")

      val getter = Getter(tree).createAndEnterSymbol()

      tree.symbol = (
        if (mods.isLazy) enterLazyVal(tree, getter)
        else {
          if (mods.isPrivateLocal)
            context.error(tree.pos, "private[this] not allowed for case class parameters")
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
        val sym = (
          owner.newMethod(tree.pos.focus, name)
            setFlag tree.mods.flags & flagsMask
            setFlag flagsExtra
        )
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
        val id = List(mods.defaultFlagString, basisSym.accurateKindString, basisSym.getterName) filterNot (_ == "") mkString " "
        log("[+derived] " + id + " (" + derivedSym + ")\n        " + result)
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
          context.error(derivedSym.pos, derivedSym+" is defined twice")

        super.validate()
      }
      // keep type tree of original abstract field
      private def fixTypeTree(dd: DefDef): DefDef = {
        dd.tpt match {
          case tt: TypeTree if dd.rhs == EmptyTree  =>
            tt setOriginal tree.tpt
          case tpt =>
            tpt setPos tree.tpt.pos.focus
        }
        dd
      }
      override def derivedTree: DefDef = {
        fixTypeTree {
          DefDef(derivedSym,
            if (mods.isDeferred) EmptyTree
            else gen.mkCheckInit(fieldSelection)
          )
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
      def name       = bean + tree.name.toString.capitalize
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
          context.error(tree.pos,
            "implementation limitation: the BeanProperty annotation cannot be used in a type alias or renamed import")
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
          context.error(tree.pos, "`BeanProperty' annotation can be applied only to fields that start with a letter")
        else if (mods.isPrivate)  // avoids name clashes with private fields in traits
          context.error(tree.pos, "`BeanProperty' annotation can be applied only to non-private fields")

        // Create and enter the symbols here, add the trees in finishGetterSetter.
        beans foreach (_.createAndEnterSymbol())
      }
    }
  }
}
