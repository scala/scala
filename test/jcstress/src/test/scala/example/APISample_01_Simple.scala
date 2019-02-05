package example

/*
 * Copyright (c) 2016, Red Hat Inc.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

import java.util.concurrent.atomic._

import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.infra.results.II_Result

/*
    This is our first concurrency test. It is deliberately simplistic to show
    testing approaches, introduce JCStress APIs, etc.
    Suppose we want to see if the field increment is atomic. We can make test
    with two actors, both actors incrementing the field and recording what
    value they observed into the result object. As JCStress runs, it will
    invoke these methods on the objects holding the field once per each actor
    and instance, and record what results are coming from there.
    Done enough times, we will get the history of observed results, and that
    would tell us something about the concurrent behavior. For example, running
    this test would yield:
          [OK] o.o.j.t.JCStressSample_01_Simple
        (JVM args: [-server])
      Observed state   Occurrences   Expectation  Interpretation
                1, 1    54,734,140    ACCEPTABLE  Both threads came up with the same value: atomicity failure.
                1, 2    47,037,891    ACCEPTABLE  actor1 incremented, then actor2.
                2, 1    53,204,629    ACCEPTABLE  actor2 incremented, then actor1.
     How to run this test:
       $ java -jar jcstress-samples/target/jcstress.jar -t JCStress_APISample_01_Simple
 */

// Mark the class as JCStress test.
@JCStressTest
@Description("Simple test, checking AtomicInteger")
@Outcomes(Array(
    new Outcome(
      id = Array("1, 1"), 
      expect = Expect.ACCEPTABLE_INTERESTING, 
      desc = "Both actors came up with the same value: atomicity failure."),
    new Outcome(
      id = Array("1, 2"), 
      expect = Expect.ACCEPTABLE, 
      desc = "actor1 incremented, then actor2."),
    new Outcome(id = Array("2, 1"), 
      expect = Expect.ACCEPTABLE, desc = "actor2 incremented, then actor1.")
  )
)
@State
class APISample_01_Simple {

  var v = new AtomicInteger(0)

  @Actor
  def actor1(r: II_Result): Unit = {
    r.r1 = v.incrementAndGet() // record result from actor1 to field r1
  }

  @Actor
  def actor2(r: II_Result): Unit = {
    r.r2 = v.incrementAndGet() // record result from actor1 to field r1
  }

}
