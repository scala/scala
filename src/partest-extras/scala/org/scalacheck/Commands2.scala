/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2014 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

private[scalacheck] trait Commands2 {

  /** The abstract state type. Must be immutable.
   *  The [[Commands2.State]] type should model the state of the system under test (SUT).
   *  It should leave out all details that aren't needed for specifying our
   *  pre- and postconditions. */
  type State

  /** A type representing one instance of the system under test (SUT).
   *  The [[Commands2.System]] type should be a proxy to the actual system under test.
   *  It is used in the postconditions to verify that the real system
   *  behaves according to specification. It should be possible to have
   *  up to [[Commands2.maxSystemInstanceCount]] co-existing instances of the System
   *  type, and each System instance should be a proxy to a distinct
   *  SUT instance. There should be no dependencies between the System
   *  instances, as they might be used in parallel by ScalaCheck.
   *  System instances are created by [[Commands2.newSystemInstance]] and destroyed by
   *  [[Commands2.destroySystemInstance]]. [[Commands2.newSystemInstance]] and
   *  [[Commands2.destroySystemInstance]] might be called at any time by ScalaCheck,
   *  as long as [[Commands2.maxSystemInstanceCount]] isn't violated. */
  type System

  /** The maximum number of concurrent [[Commands2.System]] instances allowed to exist. */
  def maxSystemInstanceCount: Int

  /** Should create a new [[Commands2.System]] instance with an internal state that
   *  corresponds to the provided abstract state instance. The provided state
   *  is guaranteed to fulfill [[Commands2.initialPreCondition]], and
   *  [[Commands2.newSystemInstance]] will never be called if there already
   *  is [[Commands2.maxSystemInstanceCount]] instances of [[Commands2.System]] */
  def newSystemInstance(state: State): System

  /** Should destroy the given SUT, so that a new [[Commands2.System]] instance can be
   *  created with [[Commands2.newSystemInstance]]. */
  def destroySystemInstance(system: System): Unit

  /** The precondition for the initial state, when no commands yet have
   *  run. This is used by ScalaCheck when command sequences are shrinked
   *  and the first state might differ from what is returned from
   *  [[Commands2.initialState]]. */
  def initialPreCondition(state: State): Boolean

  /** A generator that should produce an initial [[Commands2.State]] instance that is
   *  usable by [[Commands2.newSystemInstance]] to create a new system under test.
   *  The state returned by this generator is always checked with the
   *  [[Commands2.initialPreCondition]] method before it is used. */
  def genInitialState: Gen[State]

  /** A generator that, given the current abstract state, should produce
   *  a suitable Command instance. */
  def genCommand(state: State): Gen[Command]

  /** Abstract commands are defined as subtypes of the trait [[Commands2.Command]].
   *  Each command must have a run method and a method
   *  that returns the new abstract state, as it is supposed to look after
   *  the command has been run. A command can also define a precondition
   *  that defines how the current abstract state must look if the command
   *  should be allowed to run. Finally, you can also define a postcondition
   *  that verifies that the system under test is in a correct state after
   *  the command execution. */
  trait Command {
    /** Runs this command in the system under test,
     *  represented by the provided [[Commands2.System]] instance. This method
     *  can return any value as result. The returned value will be
     *  used by the postcondition to decide if the system behaves as
     *  expected. */
    def run(state: State, system: System): Any

    /** Returns a new abstract [[Commands2.State]] instance that represents the
     *  state of the system after this command has run. */
    def nextState(state: State): State

    /** The precondition that decides if this command is allowed to run
     *  when the system under test is in the specified (abstract) state. */
    def preCondition(state: State): Boolean

    /** The postcondition that decides if the system under test behaved
     *  correctly when the command ran.
     *  @param s0 The abstract state as it looked before this command ran.
     *  @param s1 The abstract state as it looked after this command ran.
     *  @param system The proxy for the system under test. The postcondition
     *  can query the system for its current state, but care must be taken
     *  not to mutate the system under test in any way.
     *  @param result The result returned from the [[Command.run]] method.
     */
    def postCondition(s0: State, s1: State, system: System, result: Any): Prop
  }

/* WIP
  private case class Cmds(cs: List[Command], ss: List[State]) {
    override def toString = cs.map(_.toString).mkString(", ")
  }

  private val bindings = new scala.collection.mutable.ListBuffer[(State,Any)]

  private def initState() = {
    bindings.clear()
    initialState()
  }

  private def genCmds: Gen[Cmds] = {
    def sizedCmds(s: State, sz: Int): Gen[Cmds] = {
      if(sz <= 0) Gen.const(Cmds(Nil, Nil)) else for {
        c <- genCommand(s) suchThat (_.preCondition(s))
        Cmds(cs,ss) <- sizedCmds(c.nextState(s), sz-1)
      } yield Cmds(c::cs, s::ss)
    }

    Gen.sized(sz => sizedCmds(initialState(), sz))
  }

  private def validCmds(s: State, cs: List[Command]): Option[Cmds] =
    cs match {
      case Nil => Some(Cmds(Nil, s::Nil))
      case c::_ if !c.preCondition(s) => None
      case c::cmds => for {
        Cmds(_, ss) <- validCmds(c.nextState(s), cmds)
      } yield Cmds(cs, s::ss)
    }

  private def runCommands(cmds: Cmds): Prop = Prop.all {
    cmds.cs.indices.map { i =>
      val (c,s) = (cmds.cs(i), cmds.ss(i))
      c.postCondition(s,c.nextState(s),c.run_(s))
    } : _*
  }

  private def commandsProp: Prop = {
    def shrinkCmds(cmds: Cmds) =
      Shrink.shrink(cmds.cs)(Shrink.shrinkContainer).flatMap { cs =>
        validCmds(initialState(), cs).toList
      }

    Prop.forAllShrink(genCmds label "COMMANDS", shrinkCmds)(runCommands _)
  }

  def apply(p: Prop.Params) = commandsProp(p)
*/
}
