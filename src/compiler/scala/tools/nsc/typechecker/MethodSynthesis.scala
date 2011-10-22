/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */
package scala.tools.nsc
package typechecker

import symtab.Flags._

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

  trait MethodSynth {
    self: Namer =>

    private def createAccessorSymbol(tree: ValDef, name: Name, mask: Long): TermSymbol = (
      context.owner.newMethod(tree.pos.focus, name) setFlag tree.mods.flags & mask
    )

    // TODO
    object Derived {
      def apply(tree: Tree): Derived = tree match {
        case vd @ ValDef(mods, name, _, _) =>
          if (mods hasAnnotationNamed tpnme.BeanPropertyAnnot)
            BeanGetter(vd)
          else if (mods hasAnnotationNamed tpnme.BooleanBeanPropertyAnnot)
            BooleanBeanGetter(vd)
          else
            NoDerived
        case _ => NoDerived
      }
    }
    object NoDerived extends Derived {
      def name                   = nme.NO_NAME
      def flagsMask              = 0L
      def flagsExtra             = 0L
      def completer(sym: Symbol) = NoType
    }
    trait Derived {
      def name: TermName
      def flagsMask: Long
      def flagsExtra: Long
      def completer(sym: Symbol): Type
    }
    trait DerivedAccessor extends Derived {
      def tree: ValDef
      def isSetter: Boolean
      def completer(sym: Symbol) = namerOf(sym).accessorTypeCompleter(tree, isSetter)
      def enterAccessor(): Symbol = {
        val sym = createAccessorSymbol(tree, name, flagsMask) setFlag flagsExtra
        setPrivateWithin(tree, sym)
        enterInScope(sym)
        sym setInfo completer(sym)
      }
    }
    case class Getter(tree: ValDef) extends DerivedAccessor {
      def name       = tree.name
      def isSetter   = false
      def flagsMask  = GetterFlags
      def flagsExtra = ACCESSOR | ( if (tree.mods.isMutable) 0 else STABLE )
    }
    case class Setter(tree: ValDef) extends DerivedAccessor {
      def name       = nme.getterToSetter(tree.name)
      def isSetter   = true
      def flagsMask  = SetterFlags
      def flagsExtra = ACCESSOR
    }
    case class Field(tree: ValDef) extends DerivedAccessor {
      def name       = nme.getterToLocal(tree.name)
      def isSetter   = false
      def flagsMask  = FieldFlags
      def flagsExtra = PrivateLocal
    }

    sealed abstract class BeanAccessor(bean: String, val isSetter: Boolean) extends DerivedAccessor {
      def name      = bean + tree.name.toString.capitalize
      def flagsMask = BeanPropertyFlags
      def flagsExtra = 0
    }
    case class BooleanBeanGetter(tree: ValDef) extends BeanAccessor("is", false)
    case class BeanGetter(tree: ValDef) extends BeanAccessor("get", false)
    case class BeanSetter(tree: ValDef) extends BeanAccessor("set", true)
  }
}