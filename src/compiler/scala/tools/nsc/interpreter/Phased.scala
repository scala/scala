/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

/** Mix this into an object and use it as a phasing
 *  swiss army knife.
 */
trait Phased {
  val global: Global
  import global._

  private var active: PhaseName = Typer
  private var multi: Seq[PhaseName] = Nil

  def get = active
  def set(phase: PhaseName): Boolean = phase match {
    case NoPhaseName  => false
    case name         => active = name ; true
  }
  def getMulti = multi
  def setMulti(phases: Seq[PhaseName]): Boolean = {
    if (phases contains NoPhaseName) false
    else {
      multi = phases
      true
    }
  }

  def apply[T](body: => T): T = atPhase(get)(body)
  def multi[T](body: => T): Seq[T] = multi map (ph => at(ph)(body))
  def all[T](body: => T): Seq[T] = ats(PhaseName.all)(body)

  def at[T](ph: PhaseName)(body: => T): T = {
    val saved = get
    set(ph)
    try apply(body)
    finally set(saved)
  }
  def ats[T](phs: Seq[PhaseName])(body: => T): Seq[T] = {
    val saved = multi
    setMulti(phs)
    try multi(body)
    finally setMulti(saved)
  }

  def atshow[T](phs: Seq[PhaseName])(body: => T): Unit =
    atz[T](phs)(body) foreach {
      case (ph, op) => Console.println("%15s -> %s".format(ph, op.toString take 240))
    }

  def atz[T](phs: Seq[PhaseName])(body: => T): Seq[(PhaseName, T)] =
    phs zip ats(phs)(body)

  object PhaseName {
    lazy val all = List(
      Parser, Namer, Packageobjects, Typer, Superaccessors, Pickler, Refchecks,
      Selectiveanf, Liftcode, Selectivecps, Uncurry, Tailcalls, Specialize,
      Explicitouter, Erasure, Lazyvals, Lambdalift, Constructors, Flatten, Mixin,
      Cleanup, Icode, Inliner, Closelim, Dce, Jvm, Terminal
    )
    lazy val nameMap = all.map(x => x.name -> x).toMap withDefaultValue NoPhaseName
    multi = all

    implicit def apply(s: String): PhaseName = nameMap(s)
    implicit def defaultPhaseName: PhaseName = active
  }
  sealed abstract class PhaseName {
    lazy val id   = phase.id
    lazy val name = toString.toLowerCase
    def phase     = currentRun.phaseNamed(name)

    // Execute some code during this phase.
    def apply[T](body: => T): T = atPhase(phase)(body)
  }

  case object Parser extends PhaseName
  case object Namer extends PhaseName
  case object Packageobjects extends PhaseName
  case object Typer extends PhaseName
  case object Superaccessors extends PhaseName
  case object Pickler extends PhaseName
  case object Refchecks extends PhaseName
  case object Selectiveanf extends PhaseName
  case object Liftcode extends PhaseName
  case object Selectivecps extends PhaseName
  case object Uncurry extends PhaseName
  case object Tailcalls extends PhaseName
  case object Specialize extends PhaseName
  case object Explicitouter extends PhaseName
  case object Erasure extends PhaseName
  case object Lazyvals extends PhaseName
  case object Lambdalift extends PhaseName
  case object Constructors extends PhaseName
  case object Flatten extends PhaseName
  case object Mixin extends PhaseName
  case object Cleanup extends PhaseName
  case object Icode extends PhaseName
  case object Inliner extends PhaseName
  case object Closelim extends PhaseName
  case object Dce extends PhaseName
  case object Jvm extends PhaseName
  case object Terminal extends PhaseName
  case object NoPhaseName extends PhaseName {
    override lazy val id   = -1
    override lazy val name = phase.name
    override def phase     = NoPhase
  }

  implicit def phaseEnumToPhase(name: PhaseName): Phase = name.phase
  implicit def phaseNameToPhase(name: String): Phase = currentRun.phaseNamed(name)
}
