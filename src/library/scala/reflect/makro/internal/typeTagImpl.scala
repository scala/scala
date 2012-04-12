package scala.reflect.makro

import scala.reflect.api.Universe

/** This package is required by the compiler and <b>should not be used in client code</b>. */
package object internal {
  /** This method is required by the compiler and <b>should not be used in client code</b>. */
  def materializeClassTag[T](u: Universe): ClassTag[T] = macro materializeClassTag_impl[T]

  /** This method is required by the compiler and <b>should not be used in client code</b>. */
  def materializeClassTag_impl[T: c.TypeTag](c: Context)(u: c.Expr[Universe]): c.Expr[ClassTag[T]] =
    c.Expr[Nothing](c.materializeClassTag(u.tree, implicitly[c.TypeTag[T]].tpe))(c.TypeTag.Nothing)

  /** This method is required by the compiler and <b>should not be used in client code</b>. */
  def materializeTypeTag[T](u: Universe): u.TypeTag[T] = macro materializeTypeTag_impl[T]

  /** This method is required by the compiler and <b>should not be used in client code</b>. */
  def materializeTypeTag_impl[T: c.TypeTag](c: Context)(u: c.Expr[Universe]): c.Expr[u.value.TypeTag[T]] =
    c.Expr[Nothing](c.materializeTypeTag(u.tree, implicitly[c.TypeTag[T]].tpe, requireConcreteTypeTag = false))(c.TypeTag.Nothing)

  /** This method is required by the compiler and <b>should not be used in client code</b>. */
  def materializeConcreteTypeTag[T](u: Universe): u.ConcreteTypeTag[T] = macro materializeConcreteTypeTag_impl[T]

  /** This method is required by the compiler and <b>should not be used in client code</b>. */
  def materializeConcreteTypeTag_impl[T: c.TypeTag](c: Context)(u: c.Expr[Universe]): c.Expr[u.value.ConcreteTypeTag[T]] =
    c.Expr[Nothing](c.materializeTypeTag(u.tree, implicitly[c.TypeTag[T]].tpe, requireConcreteTypeTag = true))(c.TypeTag.Nothing)

  /** This method is required by the compiler and <b>should not be used in client code</b>. */
  private[scala] implicit def context2utils(c0: Context) : Utils { val c: c0.type } = new { val c: c0.type = c0 } with Utils
}

package internal {
  private[scala] abstract class Utils {
    val c: Context

    import c.mirror._
    import definitions._

    val coreTags = Map(
      ByteClass.asType -> newTermName("Byte"),
      ShortClass.asType -> newTermName("Short"),
      CharClass.asType -> newTermName("Char"),
      IntClass.asType -> newTermName("Int"),
      LongClass.asType -> newTermName("Long"),
      FloatClass.asType -> newTermName("Float"),
      DoubleClass.asType -> newTermName("Double"),
      BooleanClass.asType -> newTermName("Boolean"),
      UnitClass.asType -> newTermName("Unit"),
      AnyClass.asType -> newTermName("Any"),
      ObjectClass.asType -> newTermName("Object"),
      AnyValClass.asType -> newTermName("AnyVal"),
      AnyRefClass.asType -> newTermName("AnyRef"),
      NothingClass.asType -> newTermName("Nothing"),
      NullClass.asType -> newTermName("Null"))

    def materializeClassTag(prefix: Tree, tpe: Type): Tree = {
      val typetagInScope = c.inferImplicitValue(appliedType(typeRef(prefix.tpe, ConcreteTypeTagClass, Nil), List(tpe)))
      def typetagIsSynthetic(tree: Tree) = tree.isInstanceOf[Block] || (tree exists (sub => sub.symbol == TypeTagModule || sub.symbol == ConcreteTypeTagModule))
      typetagInScope match {
        case success if !success.isEmpty && !typetagIsSynthetic(success) =>
          val factory = TypeApply(Select(Ident(ClassTagModule), newTermName("apply")), List(TypeTree(tpe)))
          Apply(factory, List(Select(typetagInScope, newTermName("tpe"))))
        case _ =>
          val result =
            tpe match {
              case coreTpe if coreTags contains coreTpe =>
                Select(Ident(ClassTagModule), coreTags(coreTpe))
              case _ =>
                if (tpe.typeSymbol == ArrayClass) {
                  val componentTpe = tpe.typeArguments(0)
                  val classtagInScope = c.inferImplicitValue(appliedType(typeRef(NoPrefix, ClassTagClass, Nil), List(componentTpe)))
                  val componentTag = classtagInScope orElse materializeClassTag(prefix, componentTpe)
                  Select(componentTag, newTermName("wrap"))
                } else {
                  // [Eugene] what's the intended behavior? there's no spec on ClassManifests
                  // for example, should we ban Array[T] or should we tag them with Array[AnyRef]?
                  // if its the latter, what should be the result of tagging Array[T] where T <: Int?
                  if (tpe.typeSymbol.isAbstractType) fail("tpe is an abstract type")
                  val erasure =
                    if (tpe.typeSymbol.isDerivedValueClass) tpe // [Eugene to Martin] is this correct?
                    else tpe.erasure.normalize // necessary to deal with erasures of HK types
                  val factory = TypeApply(Select(Ident(ClassTagModule), newTermName("apply")), List(TypeTree(tpe)))
                  Apply(factory, List(TypeApply(Ident(newTermName("classOf")), List(TypeTree(erasure)))))
                }
            }
          try c.typeCheck(result)
          catch { case terr @ c.TypeError(pos, msg) => fail(terr) }
      }
    }

    def materializeTypeTag(prefix: Tree, tpe: Type, requireConcreteTypeTag: Boolean): Tree = {
      val tagModule = if (requireConcreteTypeTag) ConcreteTypeTagModule else TypeTagModule
      val result =
        tpe match {
          case coreTpe if coreTags contains coreTpe =>
            Select(Select(prefix, tagModule.name), coreTags(coreTpe))
          case _ =>
            try c.reifyType(prefix, tpe, dontSpliceAtTopLevel = true, requireConcreteTypeTag = requireConcreteTypeTag)
            catch {
              case ex: Throwable =>
                // [Eugene] cannot pattern match on an abstract type, so had to do this
                val ex1 = ex
                if (ex.getClass.toString.endsWith("$ReificationError")) {
                  ex match {
                    case c.ReificationError(pos, msg) =>
                      c.error(pos, msg)
                      EmptyTree
                  }
                } else if (ex.getClass.toString.endsWith("$UnexpectedReificationError")) {
                  ex match {
                    case c.UnexpectedReificationError(pos, err, cause) =>
                      if (cause != null) throw cause else throw ex
                  }
                } else {
                  throw ex
                }
            }
        }
      try c.typeCheck(result)
      catch { case terr @ c.TypeError(pos, msg) => fail(terr) }
    }

    private def fail(reason: Any): Nothing = {
      val Apply(TypeApply(fun, List(tpeTree)), _) = c.macroApplication
      val tpe = tpeTree.tpe
      val PolyType(_, MethodType(_, tagTpe)) = fun.tpe
      val tagModule = tagTpe.typeSymbol.companionSymbol
      if (c.compilerSettings.contains("-Xlog-implicits"))
        c.echo(c.enclosingPosition, "cannot materialize " + tagModule.name + "[" + tpe + "] because:\n" + reason)
      c.abort(c.enclosingPosition, "No %s available for %s".format(tagModule.name, tpe))
    }
  }
}
