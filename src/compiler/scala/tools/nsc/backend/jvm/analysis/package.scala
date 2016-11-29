package scala.tools.nsc.backend.jvm

/**
 * Summary on the ASM analyzer framework
 * --------------------------------------
 *
 * Value
 *  - Abstract, needs to be implemented for each analysis.
 *  - Represents the desired information about local variables and stack values, for example:
 *    - Is this value known to be null / not null?
 *    - What are the instructions that could potentially have produced this value?
 *
 * Interpreter
 *  - Abstract, needs to be implemented for each analysis. Sometimes one can subclass an existing
 *    interpreter, e.g., SourceInterpreter or BasicInterpreter.
 *  - Multiple abstract methods that receive an instruction and the instruction's input values, and
 *    return a value representing the result of that instruction.
 *    - Note: due to control flow, the interpreter can be invoked multiple times for the same
 *      instruction, until reaching a fixed point.
 *  - Abstract `merge` function that computes the least upper bound of two values. Used by
 *    Frame.merge (see below).
 *
 * Frame
 *  - Can be used directly for many analyses, no subclass required.
 *  - Every frame has an array of values: one for each local variable and for each stack slot.
 *    - A `top` index stores the index of the current stack top
 *    - NOTE: for a size-2 local variable at index i, the local variable at i+1 is set to an empty
 *      value. However, for a size-2 value at index i on the stack, the value at i+1 holds the next
 *      stack value. IMPORTANT: this is only the case in ASM's analysis framework, not in bytecode.
 *      See comment below.
 *  - Defines the `execute(instruction)` method.
 *    - executing mutates the state of the frame according to the effect of the instruction
 *      - pop consumed values from the stack
 *      - pass them to the interpreter together with the instruction
 *      - if applicable, push the resulting value on the stack
 *  - Defines the `merge(otherFrame)` method
 *    - called by the analyzer when multiple control flow paths lead to an instruction
 *      - the frame at the branching instruction is merged into the current frame of the
 *        instruction (held by the analyzer)
 *      - mutates the values of the current frame, merges all values using interpreter.merge.
 *
 * Analyzer
 *   - Stores a frame for each instruction
 *   - `merge` function takes an instruction and a frame, merges the existing frame for that instr
 *     (from the frames array) with the new frame passed as argument.
 *     if the frame changed, puts the instruction on the work queue (fixpoint).
 *   - initial frame: initialized for first instr by calling interpreter.new[...]Value
 *     for each slot (locals and params), stored in frames[firstInstr] by calling `merge`
 *   - work queue of instructions (`queue` array, `top` index for next instruction to analyze)
 *   - analyze(method): simulate control flow. while work queue non-empty:
 *     - copy the state of `frames[instr]` into a local frame `current`
 *     - call `current.execute(instr, interpreter)`, mutating the `current` frame
 *     - if it's a branching instruction
 *       - for all potential destination instructions
 *         - merge the destination instruction frame with the `current` frame
 *           (this enqueues the destination instr if its frame changed)
 *       - invoke `newControlFlowEdge` (see below)
 *   - the analyzer also tracks active exception handlers at each instruction
 *   - the empty method `newControlFlowEdge` can be overridden to track control flow if required
 *
 *
 * MaxLocals and MaxStack
 * ----------------------
 *
 * At the JVM level, long and double values occupy two slots, both as local variables and on the
 * stack, as specified in the JVM spec 2.6.2:
 *   "At any point in time, an operand stack has an associated depth, where a value of type long or
 *    double contributes two units to the depth and a value of any other type contributes one unit."
 *
 * For example, a method
 *   class A { def f(a: Long, b: Long) = a + b }
 * has MAXSTACK=4 in the classfile. This value is computed by the ClassWriter / MethodWriter when
 * generating the classfile (we always pass COMPUTE_MAXS to the ClassWriter).
 *
 * For running an ASM Analyzer, long and double values occupy two local variable slots, but only
 * a single slot on the call stack, as shown by the following snippet:
 *
 *   import scala.tools.nsc.backend.jvm._
 *   import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._
 *   import scala.collection.convert.decorateAsScala._
 *   import scala.tools.asm.tree.analysis._
 *
 *   val cn = AsmUtils.readClass("/Users/luc/scala/scala/sandbox/A.class")
 *   val m = cn.methods.iterator.asScala.find(_.name == "f").head
 *
 *   // the value is read from the classfile, so it's 4
 *   println(s"maxLocals: ${m.maxLocals}, maxStack: ${m.maxStack}") // maxLocals: 5, maxStack: 4
 *
 *   // we can safely set it to 2 for running the analyzer.
 *   m.maxStack = 2
 *
 *   val a = new Analyzer(new BasicInterpreter)
 *   a.analyze(cn.name, m)
 *   val addInsn = m.instructions.iterator.asScala.find(_.getOpcode == 97).get // LADD Opcode
 *   val addFrame = a.frameAt(addInsn, m)
 *
 *   addFrame.getStackSize // 2: the two long values only take one slot each
 *   addFrame.getLocals    // 5: this takes one slot, the two long parameters take 2 slots each
 *
 *
 * While running the optimizer, we need to make sure that the `maxStack` value of a method is
 * large enough for running an ASM analyzer. We don't need to worry if the value is incorrect in
 * the JVM perspective: the value will be re-computed and overwritten in the ClassWriter.
 *
 *
 * Lessons learnt while benchmarking the alias tracking analysis
 * -------------------------------------------------------------
 *
 * Profiling
 *  - Use YourKit for finding hotspots (cpu profiling). when it comes to drilling down into the details
 *    of a hotspot, don't pay too much attention to the percentages / time counts.
 *  - Should also try other profilers.
 *  - Use timers. When a method showed up as a hotspot, i added a timer around that method, and a
 *    second one within the method to measure specific parts. The timers slow things down, but the
 *    relative numbers show what parts of a method are slow.
 *
 * ASM analyzer insights
 *  - The time for running an analysis depends on the number of locals and the number of instructions.
 *    Reducing the number of locals helps speeding up the analysis: there are less values to
 *    merge when merging to frames.
 *    See also https://github.com/scala/scala-dev/issues/47
 *  - The common hot spot of an ASM analysis is Frame.merge, for example in producers / consumers.
 *  - For nullness analysis the time is spent as follows
 *    - 20% merging nullness values. this is as expected: for example, the same absolute amount of
 *      time is spent in merging BasicValues when running a BasicInterpreter.
 *    - 50% merging alias sets. i tried to optimize what i could out of this.
 *    - 20% is spent creating new frames from existing ones, see comment on AliasingFrame.init.
 *  - The implementation of Frame.merge (the main hot spot) contains a megamorphic callsite to
 *    `interpreter.merge`. This can be observed easily by running a test program that either runs
 *    a BasicValue analysis only, versus a program that first runs a nullness analysis and then
 *    a BasicValue. In an example, the time for the BasicValue analysis goes from 519ms to 1963ms,
 *    a 3.8x slowdown.
 *  - I added counters to the Frame.merge methods for nullness and BasicValue analysis. In the
 *    examples I benchmarked, the number of merge invocations was always exactly the same.
 *    It would probably be possible to come up with an example where alias set merging forces
 *    additional analysis rounds until reaching the fixpoint, but I did not observe such cases.
 *
 * To benchmark an analysis, instead of benchmarking analysis while it runs in the compiler
 * backend, one can easily run it from a separate program (or the repl). The bytecode to analyze
 * can simply be parsed from a classfile. See example at the end of this comment.
 *
 *
 * Nullness Analysis in Miguel's Optimizer
 * ---------------------------------------
 *
 * Miguel implemented alias tracking for nullness analysis differently [1]. Remember that every
 * frame has an array of values. Miguel's idea was to represent aliasing using reference equality
 * in the values array: if two entries in the array point to the same value object, the two entries
 * are aliases in the frame of the given instruction.
 *
 * While this idea seems elegant at first sight, Miguel's implementation does not merge frames
 * correctly when it comes to aliasing. Assume in frame 1, values (a, b, c) are aliases, while in
 * frame 2 (a, b) are aliases. When merging the second into the first, we have to make sure that
 * c is removed as an alias of (a, b).
 *
 * It would be possible to implement correct alias set merging in Miguel's approach. However, frame
 * merging is the main hot spot of analysis. The computational complexity of implementing alias set
 * merging by traversing the values array and comparing references is too high. The concrete
 * alias set representation that is used in the current implementation (see class AliasingFrame)
 * makes alias set merging more efficient.
 *
 * [1] https://github.com/scala-opt/scala/blob/opt/rebase/src/compiler/scala/tools/nsc/backend/bcode/NullnessPropagator.java
 *
 *
 * Complexity and scaling of analysis
 * ----------------------------------
 *
 * The time complexity of a data flow analysis depends on:
 *
 *   - The size of the method. The complexity factor is linear (assuming the number of locals and
 *     branching instructions remains constant). The main analysis loop runs through all
 *     instructions of a method once. Instructions are only re-enqueued if a control flow merge
 *     changes the frame at some instruction.
 *
 *   - The branching instructions. When a second (third, ..) control flow edge arrives at an
 *     instruction, the existing frame at the instruction is merged with the one computed on the
 *     new branch. If the merge function changes the existing frame, the instruction is enqueued
 *     for another analysis. This results in a merge operation for the successors of the
 *     instruction.
 *
 *   - The number of local variables. The hot spot of analysis is frame merging. The merge function
 *     iterates through the values in the frame (locals and stack values) and merges them.
 *
 * I measured the running time of an analysis for two examples:
 *   - Keep the number of locals and branching instructions constant, increase the number of
 *     instructions. The running time grows linearly with the method size.
 *   - Increase the size and number of locals in a method. The method size and number of locals
 *     grow in the same pace. Here, the running time increase is polynomial. It looks like the
 *     complexity is be #instructions * #locals^2 (see below).
 *
 * I measured nullness analysis (which tracks aliases) and a SimpleValue analysis. Nullness runs
 * roughly 5x slower (because of alias tracking) at every problem size - this factor doesn't change.
 *
 * The numbers below are for nullness. Note that the last column is constant, i.e., the running
 * time is proportional to #ins * #loc^2. Therefore we use this factor when limiting the maximal
 * method size for running an analysis.
 *
 *   #insns    #locals    time (ms)       time / #ins * #loc^2 * 10^6
 *   1305      156        34              1.07
 *   2610      311        165             0.65
 *   3915      466        490             0.57
 *   5220      621        1200            0.59
 *   6525      776        2220            0.56
 *   7830      931        3830            0.56
 *   9135      1086       6570            0.60
 *   10440     1241       9700            0.60
 *   11745     1396       13800           0.60
 *
 * As a second experiment, nullness analysis was run with varying #insns but constant #locals.
 * The last column shows linear complexity with respect to the method size (linearOffset = 2279):
 *
 *   #insns     #locals     time (ms)    (time + linearOffset) / #insns
 *   5220       621         1090         0.645
 *   6224       621         1690         0.637
 *   7226       621         2280         0.630
 *   8228       621         2870         0.625
 *   9230       621         3530         0.629
 *   10232      621         4130         0.626
 *   11234      621         4770         0.627
 *   12236      621         5520         0.637
 *   13238      621         6170         0.638
 *
 *
 * When running a BasicValue analysis, the complexity observation is the same (time is proportional
 * to #ins * #loc^2).
 *
 *
 * Measuring analysis execution time
 * ---------------------------------
 *
 * See code below.
 */

/*
object Test {
  val overwrite: Option[String] = null

  @noinline def serialize(o: AnyRef): String = null

  @noinline def deserialize(string: String): AnyRef = null

  @inline def checkRoundTrip[T <: AnyRef](instance: T)(f: T => AnyRef) {
    val result = serialize(instance)
    val reconstituted = deserialize(result).asInstanceOf[T]
    assert(f(instance) == f(reconstituted), (f(instance), f(reconstituted)))
  }

  @inline def check[T <: AnyRef](instance: => T)(prevResult: String, f: T => AnyRef = (x: T) => x) {
    // pattern match to introduce a lot of control flow, i.e., a lot of frame merges
    overwrite match {
      case Some(f) =>
      case None =>
        checkRoundTrip(instance)(f)
        assert(f(deserialize(prevResult).asInstanceOf[T]) == f(instance), instance)
        assert(prevResult == "res", instance)
    }
  }

  // @inline def fun[T <: AnyRef](instance: => T) = (x: T) => x

  def testMain(): Unit = {
    // every call to check creates quite a number of locals, and also quite a number of aliases
    // of the same value (x1). First of all, the default argument call is expanded as below. Then
    // method check is inlined, and within the body of check, checkRoundTrip and assert have
    // already been inlined as well.

    // {
    //   val x1 = () => ""
    //   val x2 = fun(x1())  // the compiler optimizes this: instead of passing `() => x1()`, it just passes x1
    //   check(x1())("", x2) // same here for x1
    // }

    check("")("")
    check("")("")
    check("")("")
    check("")("")
    check("")("") // 5
    check("")("")
    check("")("")
    check("")("")
    check("")("")
    check("")("") // 10
    check("")("")
    check("")("")
    check("")("")
    check("")("")
    check("")("") // 15
    check("")("")
    check("")("")
    check("")("")
    check("")("")
    check("")("") // 20
    check("")("")
    check("")("")
    check("")("")
    check("")("")
    check("")("") // 25
    check("")("")
    check("")("")
    check("")("")
    check("")("")
    check("")("") // 30
    check("")("")
    check("")("")
    check("")("")
    check("")("")
    check("")("") // 35
    check("")("")
    check("")("")
    check("")("")
    check("")("")
    check("")("") // 40
    // check("")("")
    // check("")("")
    // check("")("")
    // check("")("")
    // check("")("") // 45
    // check("")("")
    // check("")("")
    // check("")("")
    // check("")("")
    // check("")("") // 50
    // check("")("")
    // check("")("")
    // check("")("")
    // check("")("")
    // check("")("") // 55

    // 1000 bytecode instructions, 0 locals
    // println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10)); println((1,2,3,4,5,6,7,8,9,10));
  }

  def timed[T](f: => T): T = {
    val start = System.nanoTime()
    val r = f
    val nanos = System.nanoTime() - start
    println(s"took ${nanos/1000000}ms")
    r
  }

  def main(args: Array[String]): Unit = {
    import scala.tools.nsc.backend.jvm._
    val cn = AsmUtils.readClass("/Users/luc/scala/scala/sandbox/Test$.class")
    import scala.collection.convert.decorateAsScala._
    val m = cn.methods.iterator.asScala.find(_.name == "testMain").head

    println(s"${m.instructions.size} instructions - ${m.maxLocals} locals")

    val a = new analysis.NullnessAnalyzer
    a.analyze(cn.name, m) // warm up

    analysis.AliasingFrame.reset()
    timed(a.analyze(cn.name, m))
    analysis.AliasingFrame.timers foreach println

    println("---")

    // NOTE: if we don't run nullness analysis above (comment it out), then the BasicValue
    // analysis runs 3.5x faster. Most likely because the call to Interpreter.merge inside
    // Frame.merge is no longer megamorphic.

    import scala.tools.asm.tree.analysis._
    val ba = new Analyzer(new BasicInterpreter)
    ba.analyze(cn.name, m) // warm up

    timed(ba.analyze(cn.name, m))

    println("---")

    timed(a.analyze(cn.name, m))
  }
}
*/
package object analysis
