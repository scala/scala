package scala.reflect

import api.Universe
import makro.Context
import language.implicitConversions

// todo. unfortunately, current type inferencer doesn't infer type parameters of implicit values
// this means that during macro expansion these macros will get Nothing instead of real T
// Oh how much I'd love to implement this now, but I have to postpone this until we have a solution for type inference

/** This object is required by the compiler and <b>should not be used in client code</b>. */
object TagMaterialization {
  def materializeClassTag[T: c.TypeTag](c: Context): c.Expr[ClassTag[T]] = {
    import c.mirror._
    val tpe = implicitly[c.TypeTag[T]].tpe
    c.materializeClassTag(tpe)
  }

  def materializeTypeTag[T: c.TypeTag](c: Context { type PrefixType = Universe }): c.Expr[c.prefix.value.TypeTag[T]] = {
    import c.mirror._
    val tpe = implicitly[c.TypeTag[T]].tpe
    c.materializeTypeTag(tpe, requireConcreteTypeTag = false)
  }

  def materializeConcreteTypeTag[T: c.TypeTag](c: Context { type PrefixType = Universe }): c.Expr[c.prefix.value.ConcreteTypeTag[T]] = {
    import c.mirror._
    val tpe = implicitly[c.TypeTag[T]].tpe
    c.materializeTypeTag(tpe, requireConcreteTypeTag = true)
  }

  private implicit def context2utils(c0: Context) : Utils { val c: c0.type } = new { val c: c0.type = c0 } with Utils

  private abstract class Utils {
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

    val ReflectPackage        = staticModule("scala.reflect.package")
    val Reflect_mirror        = selectTerm(ReflectPackage, "mirror")
    val ClassTagClass         = staticClass("scala.reflect.ClassTag")
    val ClassTagErasure       = selectTerm(ClassTagClass, "erasure")
    val ClassTagModule        = staticModule("scala.reflect.ClassTag")
    val TypeTagsClass         = staticClass("scala.reflect.api.TypeTags")
    val TypeTagClass          = selectType(TypeTagsClass, "TypeTag")
    val TypeTagTpe            = selectTerm(TypeTagClass, "tpe")
    val TypeTagModule         = selectTerm(TypeTagsClass, "TypeTag")
    val ConcreteTypeTagClass  = selectType(TypeTagsClass, "ConcreteTypeTag")
    val ConcreteTypeTagModule = selectTerm(TypeTagsClass, "ConcreteTypeTag")

    def materializeClassTag(tpe: Type): Tree =
      materializeTag(c.reflectMirrorPrefix, tpe, ClassTagModule, c.reifyErasure(tpe))

    def materializeTypeTag(tpe: Type, requireConcreteTypeTag: Boolean): Tree = {
      def prefix: Tree = ??? // todo. needs to be synthesized from c.prefix
      val tagModule = if (requireConcreteTypeTag) ConcreteTypeTagModule else TypeTagModule
      materializeTag(prefix, tpe, tagModule, c.reifyType(prefix, tpe, dontSpliceAtTopLevel = true, requireConcreteTypeTag = requireConcreteTypeTag))
    }

    private def materializeTag(prefix: Tree, tpe: Type, tagModule: Symbol, materializer: => Tree): Tree = {
      val result =
        tpe match {
          case coreTpe if coreTags contains coreTpe =>
            Select(Select(prefix, tagModule.name), coreTags(coreTpe))
          case _ =>
            try materializer
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
