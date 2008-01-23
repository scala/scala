/* NSC -- new scala compiler
 * Copyright 2005-2008 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab
import nsc.ast.Trees

import util._

abstract class SymbolTable extends Names
                              with Symbols
                              with Types
                              with Scopes
                              with Definitions
                              with Constants
                              with InfoTransformers
                              with StdNames
                              with AnnotationInfos
                              with AnnotationCheckers
                              with Trees
{
  def settings: Settings
  def rootLoader: LazyType
  def log(msg: AnyRef)

  /** Are we compiling for the J2ME CLDC platform ? */
  def forCLDC: Boolean

  /** Are we compiling for Java SE ? */
  def forJVM: Boolean

  /** Are we compiling for .NET ? */
  def forMSIL: Boolean

  /** are we in a lampion presentation compiler? cannot get inIDE flag from global */
  def inIDE : Boolean = false
  protected def trackTypeIDE(sym : Symbol) : Boolean = true
  def compare(sym : Symbol, name : Name) = sym.name == name
  def verifyAndPrioritize[T](g : Symbol => Symbol)(pt : Type)(f : => T) = f
  def trackSetInfo[T <: Symbol](sym : T)(info : Type) : T = {
    sym.setInfo(info); sym
  }
  def notifyImport(what : Name, container : Type, from : Name, to : Name) : Unit = {}
  def sanitize(tree : Tree) : Tree = tree

  /** A period is an ordinal number for a phase in a run.
   *  Phases in later runs have higher periods than phases in earlier runs.
   *  Later phases have higher periods than earlier phases in the same run.
   */
  type Period = Int
  final val NoPeriod = 0

  /** An ordinal number for compiler runs. First run has number 1. */
  type RunId = Int
  final val NoRunId = 0

  private var ph: Phase = NoPhase
  private var per = NoPeriod

  final def phase: Phase = ph

  final def phase_=(p: Phase) {
    //System.out.println("setting phase to " + p)
    assert((p ne null) && p != NoPhase)
    ph = p
    per = (currentRunId << 8) + p.id
  }

  /** The current compiler run identifier. */
  def currentRunId: RunId

  /** The run identifier of the given period */
  final def runId(period: Period): RunId = period >> 8

  /** The phase identifier of the given period */
  final def phaseId(period: Period): Phase#Id = period & 0xFF

  /** The period at the start of run that includes `period' */
  final def startRun(period: Period): Period = period & 0xFFFFFF00

  /** The current period */
  final def currentPeriod: Period = {
    //assert(per == (currentRunId << 8) + phase.id)
    per
  }

  /** The phase associated with given period */
  final def phaseOf(period: Period): Phase = phaseWithId(phaseId(period))

  final def period(rid: RunId, pid: Phase#Id): Period =
    (currentRunId << 8) + pid

  /** Perform given operation at given phase */
  final def atPhase[T](ph: Phase)(op: => T): T = {
    val current = phase
    phase = ph
    val result = op
    phase = current
    result
  }

  /** The set of all installed infotransformers */
  var infoTransformers = new InfoTransformer {
    val pid = NoPhase.id
    val changesBaseClasses = true
    def transform(sym: Symbol, tpe: Type): Type = tpe
  }

  /** The phase which has given index as identifier */
  val phaseWithId: Array[Phase]

  abstract class SymbolNames {
    val JavaLang     : Name
    val Object       : Name
    val Class        : Name
    val String       : Name
    val Throwable    : Name
    val NPException  : Name // NullPointerException
    val NLRException : Name = newTermName("scala.runtime.NonLocalReturnException")
    val ValueType    : Name
    val Serializable : Name
    val BeanProperty : Name
    val Delegate     : Name
    val IOOBException: Name // IndexOutOfBoundsException
    val Code         : Name
    val BoxedNumber  : Name
    val BoxedCharacter : Name
    val BoxedBoolean : Name

    import scala.collection.mutable.HashMap
    val Boxed = new HashMap[Name, Name]
  }

  private abstract class JavaNames extends SymbolNames {
    final val JavaLang      = newTermName("java.lang")
    final val Object        = newTermName("java.lang.Object")
    final val Class         = newTermName("java.lang.Class")
    final val String        = newTermName("java.lang.String")
    final val Throwable     = newTermName("java.lang.Throwable")
    final val NPException   = newTermName("java.lang.NullPointerException")
    final val ValueType     = nme.NOSYMBOL
    final val Delegate      = nme.NOSYMBOL
    final val IOOBException = newTermName("java.lang.IndexOutOfBoundsException")
    final val BoxedNumber   = newTermName("java.lang.Number")
    final val BoxedCharacter = newTermName("java.lang.Character")
    final val BoxedBoolean = newTermName("java.lang.Boolean")

    Boxed += (nme.Boolean -> newTermName("java.lang.Boolean"))
    Boxed += (nme.Byte    -> newTermName("java.lang.Byte"))
    Boxed += (nme.Char    -> newTermName("java.lang.Character"))
    Boxed += (nme.Short   -> newTermName("java.lang.Short"))
    Boxed += (nme.Int     -> newTermName("java.lang.Integer"))
    Boxed += (nme.Long    -> newTermName("java.lang.Long"))
    Boxed += (nme.Float   -> newTermName("java.lang.Float"))
    Boxed += (nme.Double  -> newTermName("java.lang.Double"))
  }

  private class MSILNames extends SymbolNames {
    final val JavaLang      = newTermName("System")
    final val Object        = newTermName("System.Object")
    final val Class         = newTermName("System.Type")
    final val String        = newTermName("System.String")
    final val Throwable     = newTermName("System.Exception")
    final val NPException   = newTermName("System.NullReferenceException")
    final val ValueType     = newTermName("System.ValueType")
    final val Serializable  = nme.NOSYMBOL
    final val BeanProperty  = nme.NOSYMBOL
    final val Delegate      = newTermName("System.MulticastDelegate")
    final val IOOBException = newTermName("System.IndexOutOfRangeException")
    final val Code          = nme.NOSYMBOL
    final val BoxedNumber   = newTermName("System.IConvertible")
    final val BoxedCharacter = newTermName("System.IConvertible")
    final val BoxedBoolean = newTermName("System.IConvertible")

    Boxed += (nme.Boolean -> newTermName("System.Boolean"))
    Boxed += (nme.Byte    -> newTermName("System.Byte"))
    Boxed += (nme.Char    -> newTermName("System.Char"))
    Boxed += (nme.Short   -> newTermName("System.Int16"))
    Boxed += (nme.Int     -> newTermName("System.Int32"))
    Boxed += (nme.Long    -> newTermName("System.Int64"))
    Boxed += (nme.Float   -> newTermName("System.Single"))
    Boxed += (nme.Double  -> newTermName("System.Double"))
  }

  private class J2SENames extends JavaNames {
    final val Serializable  = newTermName("java.io.Serializable")
    final val BeanProperty  = newTermName("scala.reflect.BeanProperty")
    final val Code          = newTermName("scala.reflect.Code")
  }

  private class CLDCNames extends JavaNames {
    final val Serializable  = nme.NOSYMBOL
    final val BeanProperty  = nme.NOSYMBOL
    final val Code          = nme.NOSYMBOL
  }

  val sn: SymbolNames =
    if (forMSIL) new MSILNames
    else if (forCLDC) new CLDCNames
    else new J2SENames

}
