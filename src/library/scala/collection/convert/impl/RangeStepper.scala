/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection.convert
package impl

import scala.collection.{IntStepper, Stepper}

/** Implements Stepper on an integer Range.  You don't actually need the Range to do this,
  * so only the relevant parts are included.  Because the arguments are protected, they are
  * not error-checked; `Range` is required to provide valid arguments.
  */
private[collection] final class RangeStepper(protected var myNext: Int, myStep: Int, _i0: Int, _iN: Int)
extends IndexedStepperBase[IntStepper, RangeStepper](_i0, _iN)
with IntStepper {
  def nextStep(): Int =
    if (hasStep) {
      val ans = myNext
      myNext += myStep
      i0 += 1
      ans 
    }
    else Stepper.throwNSEE()
  protected def semiclone(half: Int): RangeStepper = new RangeStepper(myNext, myStep, i0, half)
  override def trySplit(): IntStepper = {
    val old_i0 = i0
    val ans = super.trySplit()
    myNext += (i0 - old_i0) * myStep
    ans
  }
}
