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


  lazy val MarkerCPSSym = definitions.getClass("scala.util.continuations.cpsSym")
  lazy val MarkerCPSTypes = definitions.getClass("scala.util.continuations.cpsParam")
  lazy val MarkerCPSSynth = definitions.getClass("scala.util.continuations.cpsSynth")

  lazy val MarkerCPSAdaptPlus = definitions.getClass("scala.util.continuations.cpsPlus")
  lazy val MarkerCPSAdaptMinus = definitions.getClass("scala.util.continuations.cpsMinus")


  lazy val Context = definitions.getClass("scala.util.continuations.ControlContext")

  lazy val ModCPS = definitions.getModule("scala.util.continuations")
  lazy val MethShiftUnit = definitions.getMember(ModCPS, "shiftUnit")
  lazy val MethShiftUnitR = definitions.getMember(ModCPS, "shiftUnitR")
  lazy val MethShift = definitions.getMember(ModCPS, "shift")
  lazy val MethShiftR = definitions.getMember(ModCPS, "shiftR")
  lazy val MethReify = definitions.getMember(ModCPS, "reify")
  lazy val MethReifyR = definitions.getMember(ModCPS, "reifyR")


  lazy val allCPSAnnotations = List(MarkerCPSSym, MarkerCPSTypes, MarkerCPSSynth,
    MarkerCPSAdaptPlus, MarkerCPSAdaptMinus)

  // annotation checker

  def filterAttribs(tpe:Type, cls:Symbol) =
    tpe.annotations.filter(_.atp.typeSymbol == cls)

  def removeAttribs(tpe:Type, cls:Symbol*) =
    tpe.withoutAnnotations.withAnnotations(tpe.annotations.filterNot(cls contains _.atp.typeSymbol))

  def removeAllCPSAnnotations(tpe: Type) = removeAttribs(tpe, allCPSAnnotations:_*)

  def linearize(ann: List[AnnotationInfo]): AnnotationInfo = {
    ann.reduceLeft { (a, b) =>
      val atp0::atp1::Nil = a.atp.normalize.typeArgs
      val btp0::btp1::Nil = b.atp.normalize.typeArgs
      val (u0,v0) = (atp0, atp1)
      val (u1,v1) = (btp0, btp1)
/*
      val (u0,v0) = (a.atp.typeArgs(0), a.atp.typeArgs(1))
      val (u1,v1) = (b.atp.typeArgs(0), b.atp.typeArgs(1))
      vprintln("check lin " + a + " andThen " + b)
*/
      vprintln("check lin " + a + " andThen " + b)
      if (!(v1 <:< u0))
        throw new TypeError("illegal answer type modification: " + a + " andThen " + b)
      // TODO: improve error message (but it is not very common)
      AnnotationInfo(appliedType(MarkerCPSTypes.tpe, List(u1,v0)),Nil,Nil)
    }
  }

  // anf transform

  def getExternalAnswerTypeAnn(tp: Type) = {
    tp.annotations.find(a => a.atp.typeSymbol == MarkerCPSTypes) match {
      case Some(AnnotationInfo(atp, _, _)) =>
        val atp0::atp1::Nil = atp.normalize.typeArgs
        Some((atp0, atp1))
      case None =>
        if (tp.hasAnnotation(MarkerCPSAdaptPlus))
          global.warning("trying to instantiate type " + tp + " to unknown cps type")
        None
    }
  }

  def getAnswerTypeAnn(tp: Type) = {
    tp.annotations.find(a => a.atp.typeSymbol == MarkerCPSTypes) match {
      case Some(AnnotationInfo(atp, _, _)) =>
        if (!tp.hasAnnotation(MarkerCPSAdaptPlus)) {//&& !tp.hasAnnotation(MarkerCPSAdaptMinus))
          val atp0::atp1::Nil = atp.normalize.typeArgs
          Some((atp0, atp1))
        } else
          None
      case None => None
    }
  }

  def hasAnswerTypeAnn(tp: Type) = {
    tp.hasAnnotation(MarkerCPSTypes) && !tp.hasAnnotation(MarkerCPSAdaptPlus) /*&&
      !tp.hasAnnotation(MarkerCPSAdaptMinus)*/
  }

  def hasSynthAnn(tp: Type) = {
    tp.annotations.exists(a => a.atp.typeSymbol == MarkerCPSSynth)
  }

  def updateSynthFlag(tree: Tree) = { // remove annotations if *we* added them (@synth present)
    if (hasSynthAnn(tree.tpe)) {
      log("removing annotation from " + tree)
      tree.setType(removeAllCPSAnnotations(tree.tpe))
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

  // cps transform

}