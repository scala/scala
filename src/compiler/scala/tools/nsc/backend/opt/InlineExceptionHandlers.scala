/* NSC -- new scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 */

package scala.tools.nsc
package backend.opt
import scala.util.control.Breaks._

/**
  * This optimization phase inlines the exception handlers so that further phases can optimize the code better
  *
  * {{{
  * try {
  *   ...
  *   if (condition)
  *     throw IllegalArgumentException("sth")
  * } catch {
  *   case e: IllegalArgumentException => <handler code>
  *   case e: ... => ...
  * }
  * }}}
  *
  * will inline the exception handler code to:
  *
  * {{{
  * try {
  *   ...
  *   if (condition)
  *     <handler code> // + jump to the end of the catch statement
  * } catch {
  *   case e: IllegalArgumentException => <handler code>
  *   case e: ... => ...
  * }
  * }}}
  *
  * Q: How does the inlining work, ICode level?
  * A: if a block contains a THROW(A) instruction AND there is a handler that takes A or a superclass of A we do:
  *    1. We duplicate the handler code such that we can transform THROW into a JUMP
  *    2. We analyze the handler to see what local it expects the exception to be placed in
  *    3. We place the exception that is thrown in the correct "local variable" slot and clean up the stack
  *    4. We finally JUMP to the duplicate handler
 *    All the above logic is implemented in InlineExceptionHandlersPhase.apply(bblock: BasicBlock)
  *
  * Q: Why do we need to duplicate the handler?
  * A: An exception might be thrown in a method that we invoke in the function and we cannot see that THROW command
  * directly. In order to catch such exceptions, we keep the exception handler in place and duplicate it in order
  * to inline its code.
  *
  * @author Vlad Ureche
  */
abstract class InlineExceptionHandlers extends SubComponent {
  import global._
  import icodes._
  import icodes.opcodes._

  val phaseName = "inlineExceptionHandlers"

  /** Create a new phase */
  override def newPhase(p: Phase) = new InlineExceptionHandlersPhase(p)

  /**
    * Inlining Exception Handlers
    */
  class InlineExceptionHandlersPhase(prev: Phase) extends ICodePhase(prev) {

    def name = phaseName

    /* This map is used to keep track of duplicated exception handlers
     * explanation: for each exception handler basic block, there is a copy of it
     * -some exception handler basic blocks might not be duplicated because they have an unknown format => Option[(...)]
     * -some exception handler duplicates expect the exception on the stack while others expect it in a local
     *   => Option[Local]
     */
    var handlerCopies: Map[BasicBlock, Option[(Option[Local], BasicBlock)]] = Map.empty
    /* This map is the inverse of handlerCopies, used to compute the stack of duplicate blocks */
    var handlerCopiesInverted: Map[BasicBlock, (BasicBlock, TypeKind)] = Map.empty

    /* Type Flow Analysis */
    val tfa: analysis.MethodTFA = new analysis.MethodTFA()
    var tfaCache: Map[Int, tfa.lattice.Elem] = Map.empty
    var analyzedMethod: IMethod = null

    /* Blocks that need to be analyzed */
    var todoBlocks: List[BasicBlock] = Nil

    /* Used only for warnings */
    var currentClass: IClass = null

    /** Apply exception handler inlining to a class */
    override def apply(c: IClass): Unit =
      if (settings.inlineHandlers.value) {
        val startTime = System.currentTimeMillis
        currentClass = c

        log("Starting " + c.toString)
        for (method <- c.methods)
          apply(method)

        log("Finished " + c.toString + "... " + (System.currentTimeMillis - startTime) + "ms")
        currentClass = null
      }

    /**
      * Apply exception handler inlining to a method
      *
      * Note: for each exception handling block, we (might) create duplicates. Therefore we iterate until we get to a
      * fixed point where all the possible handlers have been inlined.
      *
      * TODO: Should we have an inlining depth limit? A nested sequence of n try-catch blocks can lead to at most 2n
      * inlined blocks, so worst case scenario we double the size of the code
      */
    def apply(method: IMethod): Unit = {

      if (method.code ne null) {
        // create the list of starting blocks
        todoBlocks = global.icodes.linearizer.linearize(method)

        while (todoBlocks.length > 0) {
          val levelBlocks = todoBlocks
          todoBlocks = Nil
          for (bblock <- levelBlocks)
            apply(bblock) // new blocks will be added to todoBlocks
        }
      }

      // Cleanup the references after we finished the file
      handlerCopies = Map.empty
      handlerCopiesInverted = Map.empty
      todoBlocks = Nil

      // Type flow analysis cleanup
      analyzedMethod = null
      tfaCache = Map.empty
      //TODO: Need a way to clear tfa structures
    }

    /** Apply exception handler inlining to a basic block  */
    def apply(bblock: BasicBlock): Unit = {
      /*
       * The logic of this entire method:
       *  - for each basic block, we look at each instruction until we find a THROW instruction
       *  - once we found a THROW instruction, we decide if it is DECIDABLE which of handler will catch the exception
       *  (see method findExceptionHandler for more details)
       *  - if we decided there is a handler that will catch the exception, we need to replace the THROW instruction by
       *  a set of equivalent instructions:
       *    * we need to compute the static types of the stack slots
       *    * we need to clear the stack, everything but the exception instance on top (or in a local variable slot)
       *    * we need to JUMP to the duplicate exception handler
       *  - we compute the static types of the stack slots in function getTypesAtInstruction
       *  - we duplicate the exception handler (and we get back the information of whether the duplicate expects the
       *  exception instance on top of the stack or in a local variable slot)
       *  - we compute the necessary code to put the exception in its place, clear the stack and JUMP
       *  - we change the THROW exception to the new Clear stack + JUMP code
       */
      for ((instr, index) <- bblock.zipWithIndex)
        if (instr.isInstanceOf[THROW]) {

          // Decide if any handler fits this exception
          val THROW(clazz) = instr
          findExceptionHandler(toTypeKind(clazz.tpe), bblock.exceptionSuccessors) match {

            case None =>
              // nothing to do, we cannot determine statically which handler will catch the exception

            case Some((handler, caughtException)) =>
              var onStackException: TypeKind = null
              var thrownException: TypeKind = null
              log("   Replacing " + instr.toString + " in " + bblock.toString + " to new handler")

              // Solve the stack and drop the element that we already stored, which should be the exception
              // needs to be done here to be the first thing before code becomes altered
              var typeInfo = getTypesAtInstruction(bblock, index)

              // Duplicate exception handler
              duplicateExceptionHandlerWithCaching(handler) match {

                case None =>
                  log("   Could not duplicate handler for " + instr.toString + " in " + bblock.toString)

                case Some((exceptionLocalOpt, newHandler)) =>

                  var canReplaceHandler = true
                  onStackException = typeInfo.head
                  thrownException = toTypeKind(clazz.tpe)

                  // A couple of sanity checks, to make sure we don't touch code we can't safely handle
                  if (typeInfo.length < 1)                      canReplaceHandler = false
                  if (index != bblock.length - 1)               canReplaceHandler = false
                  if (!(onStackException <:< thrownException))  canReplaceHandler = false
                  // in other words: what's on the stack MUST conform to what's in the THROW(..)!

                  if (!canReplaceHandler) {

                    assert(currentClass ne null)
                    currentClass.cunit.warning(NoPosition, "Unable to inline the exception handler inside incorrect" +
                      " block:\n" + bblock.mkString("\n") + "\nwith stack: " + typeInfo.toString + " just " +
                      "before instruction index " + index)

                  } else {

                    var newCode: List[Instruction] = Nil
                    var replaceType = -1

                    // Prepare the new code to replace the THROW instruction
                    exceptionLocalOpt match {

                      // the handler duplicate expects the exception in a local: easy one :)
                      case Some(exceptionLocal) =>
                        replaceType = 1
                        val exceptionType = typeInfo.head

                        newCode ::= STORE_LOCAL(exceptionLocal)
                        while (typeInfo.length > 1) {
                          typeInfo = typeInfo.tail // in the first cycle we remove the exception Type
                          newCode ::= DROP(typeInfo.head)
                        }
                        newCode ::= JUMP(newHandler)

                      // we already have the exception on top of the stack, only need to JUMP
                      case None if (typeInfo.length == 1) =>
                        replaceType = 2
                        newCode = JUMP(newHandler) :: Nil

                      // we have the exception on top of the stack but we have other stuff on the stack
                      // create a local, load exception, clear the stack and finally store the exception on the stack
                      case None =>
                        replaceType = 3
                        val exceptionType = typeInfo.head

                        assert(currentClass ne null)
                        assert(currentClass.cunit ne null)

                        // Here we could create a single local for all exceptions of a certain type. TODO: try that.
                        val localName = currentClass.cunit.freshTermName("exception$")
                        val localType = exceptionType
                        val localSymbol = bblock.method.symbol.newValue(NoPosition, localName).setInfo(localType.toType)
                        val local = new Local(localSymbol, localType, false)
                        bblock.method.addLocal(local)

                        // Save the exception, drop the stack and place back the exception
                        newCode ::= STORE_LOCAL(local)
                        while (typeInfo.length > 1) {
                          typeInfo = typeInfo.tail // in the first cycle we remove the exception Type
                          newCode ::= DROP(typeInfo.head)
                        }
                        newCode ::= LOAD_LOCAL(local)
                        newCode ::= JUMP(newHandler)
                    }
                    // replace THROW by the new code
                    bblock.replaceInstruction(instr, newCode.reverse)

                    // notify the successors changed for the current block
                    // notify the predecessors changed for the inlined handler block
                    bblock.touched = true
                    newHandler.touched = true

                    log("   Replaced  " + instr.toString + " in " + bblock.toString + " to new handler")
                    log("OPTIMIZED[" + replaceType + "] class " + currentClass.toString + " method " +
                      bblock.method.toString + " block " + bblock.toString + " newhandler " +
                      newHandler.toString + ":\n\t\t" + onStackException.toString + " <:< " +
                      thrownException.toString + " <:< " + caughtException.toString)
                  }

              }
          }
      }
    }

    /**
      * Gets the types on the stack at a certain point in the program. Note that we want to analyze the method lazily
      * and therefore use the analyzedMethod variable
      */
    def getTypesAtInstruction(bblock: BasicBlock, index: Int): List[TypeKind] = {

      // get the stack at the block entry
      var typeInfo = getTypesAtBlockEntry(bblock)

      // perform tfa to the current instruction
      log("         stack at the beginning of block " + bblock.toString + " in function " +
        bblock.method.toString + ": " + typeInfo.stack.toString)
      for (i <- 0 to (index - 1)) {
        typeInfo = tfa.interpret(typeInfo, bblock(i))
        log("         stack after interpret: " + typeInfo.stack.toString + " after instruction " +
          bblock(i).toString)
      }
      log("         stack before instruction " + index + " of block " + bblock.toString + " in function " +
        bblock.method.toString + ": " + typeInfo.stack.toString)

      // return the result
      typeInfo.stack.types
    }


    /**
      * Gets the stack at the block entry. Normally the typeFlowAnalysis should be run again, but we know how to compute
      * the stack for handler duplicates. For the locals, it's safe to assume the info from the original handler is
      * still valid (a more precise analysis can be done, but it's not necessary)
      */
    def getTypesAtBlockEntry(bblock: BasicBlock): tfa.lattice.Elem = {

      // lazily perform tfa, because it's expensive
      // cache results by block label, as rewriting the code messes up the block's hashCode
      if (analyzedMethod eq null) {
        analyzedMethod = bblock.method
        tfa.init(bblock.method)
        tfa.run
        log("      performed tfa on method: " + bblock.method.toString)

        for (block <- bblock.method.code.blocks.sortWith(_.label < _.label))
          tfaCache += block.label -> tfa.in(block)
      }

      log("         getting typeinfo at the beginning of block " + bblock.toString)

      if (tfaCache contains bblock.label)
        tfaCache(bblock.label)
      else {
        // this block was not analyzed, but it's a copy of some other block so its stack should be the same
        log("         getting typeinfo at the beginning of block " + bblock.toString + " as a copy of " +
          handlerCopiesInverted(bblock).toString)
        val (origBlock, exception) = handlerCopiesInverted(bblock)
        val typeInfo               = getTypesAtBlockEntry(origBlock)
        val stack                  = handlerCopies(origBlock).get._1 match {
          case Some(_) => Nil             // empty stack, the handler copy expects an empty stack
          case None    => List(exception) // one slot on the stack for the exception
        }

        // If we use the mutability property, it crashes the analysis
        tfa.lattice.IState(new analysis.VarBinding(typeInfo.vars), new icodes.TypeStack(stack))
      }
    }

    /**
      * Finds the first exception handler that matches the current exception
      *
      * Note the following code:
      * {{{
      * try {
      *   throw new IllegalArgumentException("...")
      * } catch {
      *   case e: RuntimeException => log("RuntimeException")
      *   case i: IllegalArgumentException => log("IllegalArgumentException")
      * }
      * }}}
      *
      * will print "RuntimeException" => we need the *first* valid handler
      *
      * There's a hidden catch here: say we have the following code:
      * {{{
      * try {
      *   val exception: Throwable =
      *     if (scala.util.Random.nextInt % 2 == 0)
      *       new IllegalArgumentException("even")
      *     else
      *       new StackOverflowError("odd")
      *   throw exception
      * } catch {
      *   case e: IllegalArgumentException =>
      *     println("Correct, IllegalArgumentException")
      *   case e: StackOverflowError =>
      *     println("Correct, StackOverflowException")
      *   case t: Throwable =>
      *     println("WROOOONG, not Throwable!")
      * }
      * }}}
      *
      * We don't want to select a handler if there's at least one that's more specific!
      */
    def findExceptionHandler(thrownException: TypeKind, handlers: List[BasicBlock]): Option[(BasicBlock, TypeKind)] = {

      // function to extract exeption type
      def extractException(bb: BasicBlock): Option[TypeKind] =
        if (bb.length >= 1) {
          bb.head match {
            case LOAD_EXCEPTION(clazz) => Some(toTypeKind(clazz.tpe))
            case _ => None
          }
        } else
          None

      var finalHandlerData: Option[(BasicBlock, TypeKind)] = None

      breakable {
        for (handler <- handlers)
          extractException(handler) match {
            case Some(caughtException) if (thrownException <:< caughtException) =>
              // we'll do inlining here: createdException <:< thrownException <:< caughtException, good!
              finalHandlerData = Some((handler, caughtException))
              break
            case Some(caughtException) if (caughtException <:< thrownException) =>
              // we can't do inlining here, the handling mechanism is more precise than we can reason about
              finalHandlerData = None
              break
            case _ =>
              // no result yet, look deeper in the handler stack :)
          }
      }

      finalHandlerData
    }


    /**
      * This function takes care of duplicating the basic block code for inlining the handler
      *
      * Note: This function does not duplicate the same basic block twice. It wil contain a map of the duplicated
      * basic blocks
      */
    def duplicateExceptionHandlerWithCaching(handler: BasicBlock): Option[(Option[Local], BasicBlock)] = {

      if (!(handlerCopies contains handler))
        handlerCopies = handlerCopies + (handler -> duplicateExceptionHandler(handler))

      handlerCopies(handler)
    }

    /** This function takes care of actual duplication */
    def duplicateExceptionHandler(handler: BasicBlock): Option[(Option[Local], BasicBlock)] = {

      log("      duplicating handler block " + handler.toString)

      // Sanitiy checks
      var canDuplicate = true
      if (handler.length < 2)                         canDuplicate = false
      if (!(handler(0).isInstanceOf[LOAD_EXCEPTION])) canDuplicate = false

      canDuplicate match {
        case true =>

          val LOAD_EXCEPTION(caughtClass) = handler(0)
          val caughtException = toTypeKind(caughtClass.tpe)

          val exceptionLocal: Option[Local] =
            if (handler(1).isInstanceOf[STORE_LOCAL])
              STORE_LOCAL.unapply(handler(1).asInstanceOf[STORE_LOCAL])
            else
              None

          val dropIntstructions =
            if (exceptionLocal == None)
              1 // we only drop the LOAD_EXCEPTION and expect the exception on the stack
            else
              2 // we drop both LOAD_EXCEPTION and STORE_LOCAL

          // copy the exception handler code once again, dropping the LOAD_EXCEPTION
          val copy = handler.code.newBlock
          for (instr <- handler.drop(dropIntstructions))
            copy.emit(instr, instr.pos)
          copy.close

          // extend the handlers of the handler to the copy
          for (parentHandler <-handler.method.exh)
            if (parentHandler covers handler) {
              parentHandler.addCoveredBlock(copy)
              // notify the parent handler that the successors changed
              parentHandler.startBlock.touched = true
            }

          // notify the successors of the inlined handler might have changed
          copy.touched = true
          handler.touched = true

          log("      duplicated  handler block " + handler.toString + " to " + copy.toString)

          // announce the duplicate handler
          handlerCopiesInverted = handlerCopiesInverted + (copy -> ((handler, caughtException)))
          todoBlocks = copy :: todoBlocks

          Some((exceptionLocal, copy))

        case false =>
          assert(currentClass ne null)
          currentClass.cunit.warning(NoPosition, "Unable to inline the exception handler due to incorrect format:\n" +
            handler.mkString("\n"))
          None
      }
    }
  }
}
