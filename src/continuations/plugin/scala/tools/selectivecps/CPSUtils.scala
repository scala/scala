// $Id$

package scala.tools.selectivecps

import scala.tools.nsc.Global

trait CPSUtils {
  val global: Global
  import global._
  import definitions._

  var cpsEnabled = false
  val verbose: Boolean = System.getProperty("cpsVerbose", "false") == "true"
  def vprintln(x: =>Any): Unit = if (verbose) println(x)

  object cpsNames {
    val catches         = newTermName("$catches")
    val ex              = newTermName("$ex")
    val flatMapCatch    = newTermName("flatMapCatch")
    val getTrivialValue = newTermName("getTrivialValue")
    val isTrivial       = newTermName("isTrivial")
    val reify           = newTermName("reify")
    val reifyR          = newTermName("reifyR")
    val shift           = newTermName("shift")
    val shiftR          = newTermName("shiftR")
    val shiftSuffix     = newTermName("$shift")
    val shiftUnit0      = newTermName("shiftUnit0")
    val shiftUnit       = newTermName("shiftUnit")
    val shiftUnitR      = newTermName("shiftUnitR")
  }

  lazy val MarkerCPSSym        = rootMirror.getRequiredClass("scala.util.continuations.cpsSym")
  lazy val MarkerCPSTypes      = rootMirror.getRequiredClass("scala.util.continuations.cpsParam")
  lazy val MarkerCPSSynth      = rootMirror.getRequiredClass("scala.util.continuations.cpsSynth")
  lazy val MarkerCPSAdaptPlus  = rootMirror.getRequiredClass("scala.util.continuations.cpsPlus")
  lazy val MarkerCPSAdaptMinus = rootMirror.getRequiredClass("scala.util.continuations.cpsMinus")

  lazy val Context = rootMirror.getRequiredClass("scala.util.continuations.ControlContext")
  lazy val ModCPS = rootMirror.getRequiredModule("scala.util.continuations")

  lazy val MethShiftUnit  = definitions.getMember(ModCPS, cpsNames.shiftUnit)
  lazy val MethShiftUnit0 = definitions.getMember(ModCPS, cpsNames.shiftUnit0)
  lazy val MethShiftUnitR = definitions.getMember(ModCPS, cpsNames.shiftUnitR)
  lazy val MethShift      = definitions.getMember(ModCPS, cpsNames.shift)
  lazy val MethShiftR     = definitions.getMember(ModCPS, cpsNames.shiftR)
  lazy val MethReify      = definitions.getMember(ModCPS, cpsNames.reify)
  lazy val MethReifyR     = definitions.getMember(ModCPS, cpsNames.reifyR)

  lazy val allCPSAnnotations = List(MarkerCPSSym, MarkerCPSTypes, MarkerCPSSynth,
    MarkerCPSAdaptPlus, MarkerCPSAdaptMinus)

  // TODO - needed? Can these all use the same annotation info?
  protected def newSynthMarker() = newMarker(MarkerCPSSynth)
  protected def newPlusMarker()  = newMarker(MarkerCPSAdaptPlus)
  protected def newMinusMarker() = newMarker(MarkerCPSAdaptMinus)
  protected def newMarker(tpe: Type): AnnotationInfo = AnnotationInfo marker tpe
  protected def newMarker(sym: Symbol): AnnotationInfo = AnnotationInfo marker sym.tpe

  protected def newCpsParamsMarker(tp1: Type, tp2: Type) =
    newMarker(appliedType(MarkerCPSTypes.tpe, List(tp1, tp2)))

  // annotation checker

  protected def annTypes(ann: AnnotationInfo): (Type, Type) = {
    val tp0 :: tp1 :: Nil = ann.atp.normalize.typeArgs
    ((tp0, tp1))
  }
  protected def hasMinusMarker(tpe: Type)   = tpe hasAnnotation MarkerCPSAdaptMinus
  protected def hasPlusMarker(tpe: Type)    = tpe hasAnnotation MarkerCPSAdaptPlus
  protected def hasSynthMarker(tpe: Type)   = tpe hasAnnotation MarkerCPSSynth
  protected def hasCpsParamTypes(tpe: Type) = tpe hasAnnotation MarkerCPSTypes
  protected def cpsParamTypes(tpe: Type)    = tpe getAnnotation MarkerCPSTypes map annTypes

  def filterAttribs(tpe:Type, cls:Symbol) =
    tpe.annotations filter (_ matches cls)

  def removeAttribs(tpe: Type, classes: Symbol*) =
    tpe filterAnnotations (ann => !(classes exists (ann matches _)))

  def removeAllCPSAnnotations(tpe: Type) = removeAttribs(tpe, allCPSAnnotations:_*)

  def cpsParamAnnotation(tpe: Type) = filterAttribs(tpe, MarkerCPSTypes)

  def linearize(ann: List[AnnotationInfo]): AnnotationInfo = {
    ann reduceLeft { (a, b) =>
      val (u0,v0) = annTypes(a)
      val (u1,v1) = annTypes(b)
      // vprintln("check lin " + a + " andThen " + b)

      if (v1 <:< u0)
        newCpsParamsMarker(u1, v0)
      else
        throw new TypeError("illegal answer type modification: " + a + " andThen " + b)
    }
  }

  // anf transform

  def getExternalAnswerTypeAnn(tp: Type) = {
    cpsParamTypes(tp) orElse {
      if (hasPlusMarker(tp))
        global.warning("trying to instantiate type " + tp + " to unknown cps type")
      None
    }
  }

  def getAnswerTypeAnn(tp: Type): Option[(Type, Type)] =
    cpsParamTypes(tp) filterNot (_ => hasPlusMarker(tp))

  def hasAnswerTypeAnn(tp: Type) =
    hasCpsParamTypes(tp) && !hasPlusMarker(tp)

  def updateSynthFlag(tree: Tree) = { // remove annotations if *we* added them (@synth present)
    if (hasSynthMarker(tree.tpe)) {
      log("removing annotation from " + tree)
      tree modifyType removeAllCPSAnnotations
    } else
      tree
  }

  type CPSInfo = Option[(Type,Type)]

  def linearize(a: CPSInfo, b: CPSInfo)(implicit unit: CompilationUnit, pos: Position): CPSInfo = {
    (a,b) match {
      case (Some((u0,v0)), Some((u1,v1))) =>
        vprintln("check lin " + a + " andThen " + b)
        if (!(v1 <:< u0)) {
          unit.error(pos,"cannot change answer type in composition of cps expressions " +
          "from " + u1 + " to " + v0 + " because " + v1 + " is not a subtype of " + u0 + ".")
          throw new Exception("check lin " + a + " andThen " + b)
        }
        Some((u1,v0))
      case (Some(_), _) => a
      case (_, Some(_)) => b
      case _ => None
    }
  }
}
