/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package icode

import scala.collection.mutable.{Buffer, ListBuffer, Map, HashMap}
import scala.tools.nsc.symtab._

abstract class Checkers {
  val global: Global
  import global._

  /** <p>
   *    This class performs a set of checks similar to what the bytecode
   *    verifier does. For each basic block, it checks that:
   *  </p>
   *  <ul>
   *    <li>
   *      for primitive operations: the type and numer of operands match
   *      the type of the operation
   *    </li>
   *    <li>
   *      for method calls: the method exists in the type of the receiver
   *      and the number and type of arguments match the declared type of
   *      the method.
   *    </li>
   *    <li>
   *      for object creation: the constructor can be called.
   *    </li>
   *    <li>
   *      for load/stores: the field/local/param exists and the type
   *      of the value matches that of the target.
   *    </li>
   *  </ul>
   *  <p>
   *    For a control flow graph it checks that type stacks at entry to
   *    each basic block 'agree':
   *  </p>
   *  <ul>
   *    <li>they have the same length</li>
   *    <li>there exists a lub for all types at the same position in stacks.</li>
   *  </ul>
   *
   *  @author  Iulian Dragos
   *  @version 1.0, 06/09/2005
   *
   *  @todo Better checks for <code>MONITOR_ENTER/EXIT</code>
   *        Better checks for local var initializations
   */
  class ICodeChecker {
    import icodes._
    import opcodes._

    var clasz: IClass = _
    var method: IMethod = _
    var code: Code = _

    val in: Map[BasicBlock, TypeStack] = new HashMap()
    val out: Map[BasicBlock, TypeStack] = new HashMap()

    val emptyStack = new TypeStack()

    val STRING        = REFERENCE(definitions.StringClass)
    val BOXED_UNIT    = REFERENCE(definitions.BoxedUnitClass)
    val SCALA_NOTHING = REFERENCE(definitions.NothingClass)
    val SCALA_NULL    = REFERENCE(definitions.NullClass)

    /** A wrapper to route log messages to debug output also.
     */
    def logChecker(msg: String) = {
      log(msg)
      checkerDebug(msg)
    }

    def checkICodes: Unit = {
      if (settings.verbose.value)
      println("[[consistency check at the beginning of phase " + globalPhase.name + "]]")
      classes.values foreach check
    }

    /** Only called when m1 < m2, so already known that (m1 ne m2).
     */
    private def isConfict(m1: IMember, m2: IMember, canOverload: Boolean) = (
      (m1.symbol.name == m2.symbol.name) &&
      (!canOverload || (m1.symbol.tpe =:= m2.symbol.tpe))
    )

    def check(cls: IClass) {
      logChecker("\n** Checking class " + cls)
      clasz = cls

      for (f1 <- cls.fields ; f2 <- cls.fields ; if f1 < f2)
        if (isConfict(f1, f2, false))
          Checkers.this.global.error("Repetitive field name: " + f1.symbol.fullName)

      for (m1 <- cls.methods ; m2 <- cls.methods ; if m1 < m2)
        if (isConfict(m1, m2, true))
          Checkers.this.global.error("Repetitive method: " + m1.symbol.fullName)

      clasz.methods foreach check
    }

    def check(m: IMethod) {
      logChecker("\n** Checking method " + m)
      method = m
      if (!m.isDeferred)
        check(m.code)
    }

    def check(c: Code) {
      var worklist: Buffer[BasicBlock] = new ListBuffer()

      def append(elems: List[BasicBlock]) = elems foreach appendBlock;
      def appendBlock(bl: BasicBlock) =
        if (!(worklist contains bl))
          worklist += bl

      in.clear;
      out.clear;
      code = c;
      worklist += c.startBlock
      for (bl <- c.blocks) {
        in  += (bl -> emptyStack)
        out += (bl -> emptyStack)
      }

      while (worklist.nonEmpty) {
        val block = worklist(0);
        worklist.trimStart(1);
        val output = check(block, in(block));
        if (output != out(block) || (out(block) eq emptyStack)) {
          if (block.successors.nonEmpty || block.successors.nonEmpty)
            logChecker("Output changed for " + block.fullString)

          out(block) = output
          append(block.successors)
          block.successors foreach meet
        }
      }
    }

    /**
     * Apply the meet operator of the stack lattice on bl's predecessors.
     * :-). Compute the input to bl by checking that all stacks have the
     * same length, and taking the lub of types at the same positions.
     */
    def meet(bl: BasicBlock) {
      val preds = bl.predecessors

      /** XXX workaround #1: one stack empty, the other has BoxedUnit.
       *  One example where this arises is:
       *
       *  def f(b: Boolean): Unit = synchronized { if (b) () }
       */
      def isAllUnits(s1: TypeStack, s2: TypeStack) = {
        List(s1, s2) forall (x => x.types forall (_ == BOXED_UNIT))
      }
      /** XXX workaround #2: different stacks heading into an exception
       *  handler which will clear them anyway.  Examples where it arises:
       *
       *  var bippy: Int = synchronized { if (b) 5 else 10 }
       */
      def isHandlerBlock() = bl.exceptionHandlerStart

      /** The presence of emptyStack means that path has not yet been checked
       *  (and may not be empty) thus the reference eq tests.
       */
      def meet2(s1: TypeStack, s2: TypeStack): TypeStack = {
        def workaround(msg: String) = {
          checkerDebug(msg + ": " + method + " at block " + bl)
          checkerDebug("  s1: " + s1)
          checkerDebug("  s2: " + s2)
          new TypeStack()
        }

        if (s1 eq emptyStack) s2
        else if (s2 eq emptyStack) s1
        else if (s1.length != s2.length) {
          if (isAllUnits(s1, s2))
            workaround("Ignoring mismatched boxed units")
          else if (isHandlerBlock)
            workaround("Ignoring mismatched stacks entering exception handler")
          else
            throw new CheckerException("Incompatible stacks: " + s1 + " and " + s2 + " in " + method + " at entry to block: " + bl);
        }
        else {
          val newStack = new TypeStack((s1.types, s2.types).zipped map lub)
          if (newStack.nonEmpty)
            checkerDebug("Checker created new stack:\n  (%s, %s) => %s".format(s1, s2, newStack))

          newStack
        }
      }

      if (preds.nonEmpty) {
        in(bl) = (preds map out.apply) reduceLeft meet2;
        log("Input changed for block: " + bl +" to: " + in(bl));
      }
    }

    private var typeStack: TypeStack = null
    private var instruction: Instruction = null
    private var basicBlock: BasicBlock = null

    /**
     * Check the basic block to be type correct and return the
     * produced type stack.
     */
    def check(b: BasicBlock, initial: TypeStack): TypeStack = {
      logChecker({
        val prefix = "** Checking " + b.fullString

        if (initial.isEmpty) prefix
        else prefix + " with initial stack " + initial.types.mkString("[", ", ", "]")
      })

      var stack = new TypeStack(initial)
      def checkStack(len: Int) {
        if (stack.length < len)
          ICodeChecker.this.error("Expected at least " + len + " elements on the stack", stack)
      }

      def sizeString(push: Boolean) = {
        val arrow = if (push) "-> " else "<- "
        val sp    = "   " * stack.length

        sp + stack.length + arrow
      }
      def _popStack: TypeKind = {
        if (stack.isEmpty) {
          error("Popped empty stack in " + b.fullString + ", throwing a Unit")
          return UNIT
        }

        val res = stack.pop
        checkerDebug(sizeString(false) + res)
        res
      }
      def popStack     = { checkStack(1) ; _popStack }
      def popStack2    = { checkStack(2) ; (_popStack, _popStack) }
      def popStack3    = { checkStack(3) ; (_popStack, _popStack, _popStack) }
      def clearStack() = 1 to stack.length foreach (_ => popStack)

      def pushStack(xs: TypeKind*): Unit = {
        xs foreach { x =>
          if (x == UNIT) {
            /** PP: I admit I'm not yet figuring out how the stacks will balance when
             *  we ignore UNIT, but I expect this knowledge will emerge naturally.
             *  In the meantime I'm logging it to help me out.
             */
            logChecker("Ignoring pushed UNIT")
          }
          else {
            stack push x
            checkerDebug(sizeString(true) + x)
          }
        }
      }

      this.typeStack = stack
      this.basicBlock = b

      def typeError(k1: TypeKind, k2: TypeKind) {
        error("\n  expected: " + k1 + "\n     found: " + k2)
      }

      def subtypeTest(k1: TypeKind, k2: TypeKind): Unit =
        if (k1 <:< k2) ()
        else typeError(k2, k1)

      for (instr <- b) {

        def checkLocal(local: Local): Unit = {
          (method lookupLocal local.sym.name) getOrElse {
            error(" " + local + " is not defined in method " + method)
          }
        }

        def checkField(obj: TypeKind, field: Symbol) {
          obj match {
            case REFERENCE(sym) =>
              if (sym.info.member(field.name) == NoSymbol)
                error(" " + field + " is not defined in class " + clasz);
            case _ =>
              error(" expected reference type, but " + obj + " found");
          }
        }

        /** Checks that tpe is a subtype of one of the allowed types */
        def checkType(tpe: TypeKind, allowed: TypeKind*) {
          if (isOneOf(tpe, allowed: _*)) ()
          else error(tpe + " is not one of: " + allowed.mkString("{", ", ", "}"))
        }
        def checkNumeric(tpe: TypeKind) =
          checkType(tpe, BYTE, CHAR, SHORT, INT, LONG, FLOAT, DOUBLE)

        /** Checks that the 2 topmost elements on stack are of the
         *  kind TypeKind.
         */
        def checkBinop(kind: TypeKind) {
          val (a, b) = popStack2
          checkType(a, kind)
          checkType(b, kind)
        }

        /** Check that arguments on the stack match method params. */
        def checkMethodArgs(method: Symbol) {
          val params = method.info.paramTypes
          checkStack(params.length)
          params.reverse foreach (tpe => checkType(popStack, toTypeKind(tpe)))
        }

        /** Checks that the object passed as receiver has a method
         *  <code>method</code> and that it is callable from the current method.
         *
         *  @param receiver ...
         *  @param method   ...
         */
        def checkMethod(receiver: TypeKind, method: Symbol) =
          receiver match {
            case REFERENCE(sym) =>
              checkBool(sym.info.member(method.name) != NoSymbol,
                        "Method " + method + " does not exist in " + sym.fullName);
              if (method.isPrivate)
                checkBool(method.owner == clasz.symbol,
                          "Cannot call private method of " + method.owner.fullName
                          + " from " + clasz.symbol.fullName);
              else if (method.isProtected)
                checkBool(clasz.symbol isSubClass method.owner,
                          "Cannot call protected method of " + method.owner.fullName
                          + " from " + clasz.symbol.fullName);

            case ARRAY(_) =>
              checkBool(receiver.toType.member(method.name) != NoSymbol,
                        "Method " + method + " does not exist in " + receiver)

            case t =>
              error("Not a reference type: " + t)
          }

        def checkBool(cond: Boolean, msg: String) =
          if (!cond) error(msg)

        this.instruction = instr

        if (settings.debug.value) {
          log("PC: " + instr)
          log("stack: " + stack)
          log("================")
        }
        instr match {
          case THIS(clasz) =>
            pushStack(toTypeKind(clasz.tpe))

          case CONSTANT(const) =>
            pushStack(toTypeKind(const.tpe))

          case LOAD_ARRAY_ITEM(kind) =>
            popStack2 match {
              case (INT, ARRAY(elem)) =>
                subtypeTest(elem, kind)
                pushStack(elem)
              case (a, b) =>
                error(" expected and INT and a array reference, but " +
                    a + ", " + b + " found");
            }

         case LOAD_LOCAL(local) =>
           checkLocal(local)
           pushStack(local.kind)

         case LOAD_FIELD(field, isStatic) =>
           // the symbol's owner should contain it's field, but
           // this is already checked by the type checker, no need
           // to redo that here
           if (isStatic) ()
           else checkField(popStack, field)

           pushStack(toTypeKind(field.tpe))

         case LOAD_MODULE(module) =>
           checkBool((module.isModule || module.isModuleClass),
                     "Expected module: " + module + " flags: " + Flags.flagsToString(module.flags));
           pushStack(toTypeKind(module.tpe));

         case STORE_THIS(kind) =>
           val actualType = popStack
           if (actualType.isReferenceType) subtypeTest(actualType, kind)
           else error("Expected this reference but found: " + actualType)

         case STORE_ARRAY_ITEM(kind) =>
           popStack3 match {
             case (k, INT, ARRAY(elem)) =>
               subtypeTest(k, kind)
               subtypeTest(k, elem)
             case (a, b, c) =>
                error(" expected and array reference, and int and " + kind +
                      " but " + a + ", " + b + ", " + c + " found");
           }

         case STORE_LOCAL(local) =>
           checkLocal(local)
           val actualType = popStack
           // PP: ThrowableReference is temporary to deal with exceptions
           // not yet appearing typed.
           if (actualType == ThrowableReference || local.kind == SCALA_NULL) ()
           else subtypeTest(actualType, local.kind)

         case STORE_FIELD(field, true) =>     // static
           val fieldType = toTypeKind(field.tpe)
           val actualType = popStack
           subtypeTest(actualType, fieldType)

         case STORE_FIELD(field, false) =>    // not static
           val (value, obj) = popStack2
           checkField(obj, field)
           val fieldType = toTypeKind(field.tpe)
           if (fieldType == SCALA_NULL) ()
           else subtypeTest(value, fieldType)

         case CALL_PRIMITIVE(primitive) =>
           checkStack(instr.consumed)
           primitive match {
             case Negation(kind) =>
               checkType(kind, BOOL, BYTE, CHAR, SHORT, INT, LONG, FLOAT, DOUBLE)
               checkType(popStack, kind)
               pushStack(kind)

             case Test(op, kind, zero) =>
               if (zero) checkType(popStack, kind)
               else checkBinop(kind)

               pushStack(BOOL)

             case Comparison(op, kind) =>
               checkNumeric(kind)
               checkBinop(kind)
               pushStack(INT)

             case Arithmetic(op, kind) =>
               checkNumeric(kind)
               if (op == NOT)
                 checkType(popStack, kind)
               else
                 checkBinop(kind)
               pushStack(kind)

             case Logical(op, kind) =>
               checkType(kind, BOOL, BYTE, CHAR, SHORT, INT, LONG)
               checkBinop(kind)
               pushStack(kind)

             case Shift(op, kind) =>
               checkType(kind, BYTE, CHAR, SHORT, INT, LONG)
               val (a, b) = popStack2
               checkType(a, INT)
               checkType(b, kind)
               pushStack(kind)

             case Conversion(src, dst) =>
               checkNumeric(src)
               checkNumeric(dst)
               checkType(popStack, src)
               pushStack(dst)

             case ArrayLength(kind) =>
               popStack match {
                 case ARRAY(elem) => checkType(elem, kind)
                 case arr         => error(" array reference expected, but " + arr + " found")
               }
               pushStack(INT)

             case StartConcat =>
               pushStack(ConcatClass)

             case EndConcat =>
               checkType(popStack, ConcatClass)
               pushStack(STRING)

             case StringConcat(el) =>
               checkType(popStack, el)
               checkType(popStack, ConcatClass)
               pushStack(ConcatClass)
           }

         case CALL_METHOD(method, style) =>
           def paramCount     = method.info.paramTypes.length
           def pushReturnType = pushStack(toTypeKind(method.info.resultType))

           style match {
             case Dynamic | InvokeDynamic =>
               checkStack(1 + paramCount)
               checkMethodArgs(method)
               checkMethod(popStack, method)
               pushReturnType

             case Static(onInstance) =>
               if (onInstance) {
                 checkStack(1 + paramCount)
                 checkBool(method.isPrivate || method.isConstructor,
                           "Static call to non-private method.")
                 checkMethodArgs(method)
                 checkMethod(popStack, method)
                 if (!method.isConstructor)
                   pushReturnType
               }
               else {
                 checkStack(paramCount);
                 checkMethodArgs(method);
                 pushReturnType
               }

             case SuperCall(mix) =>
               checkStack(1 + paramCount)
               checkMethodArgs(method)
               checkMethod(popStack, method)
               pushReturnType
           }

          case NEW(kind) =>
            pushStack(kind)

          case CREATE_ARRAY(elem, dims) =>
            checkStack(dims)
            stack.pop(dims) foreach (checkType(_, INT))
            pushStack(ARRAY(elem))

          case IS_INSTANCE(tpe) =>
            val ref = popStack
            checkBool(!ref.isValueType, "IS_INSTANCE on primitive type: " + ref)
            checkBool(!tpe.isValueType, "IS_INSTANCE on primitive type: " + tpe)
            pushStack(BOOL)

          case CHECK_CAST(tpe) =>
            val ref = popStack
            checkBool(!ref.isValueType, "CHECK_CAST to primitive type: " + ref)
            checkBool(!tpe.isValueType, "CHECK_CAST to primitive type: " + tpe)
            pushStack(tpe)

          case SWITCH(tags, labels) =>
            checkType(popStack, INT)
            checkBool(tags.length == labels.length - 1,
                      "The number of tags and labels does not coincide.")
            checkBool(labels forall (b => code.blocks contains b),
                      "Switch target cannot be found in code.")

          case JUMP(whereto) =>
            checkBool(code.blocks contains whereto,
                      "Jump to non-existant block " + whereto)

          case CJUMP(success, failure, cond, kind) =>
            checkBool(code.blocks contains success,
                      "Jump to non-existant block " + success)
            checkBool(code.blocks contains failure,
                      "Jump to non-existant block " + failure)
            checkBinop(kind)

          case CZJUMP(success, failure, cond, kind) =>
            checkBool(code.blocks contains success,
                      "Jump to non-existant block " + success)
            checkBool(code.blocks contains failure,
                      "Jump to non-existant block " + failure)
            checkType(popStack, kind)

          case RETURN(UNIT) => ()
          case RETURN(kind) =>
            val top = popStack
            if (kind.isValueType) checkType(top, kind)
            else checkBool(!top.isValueType, "" + kind + " is a reference type, but " + top + " is not");

          case THROW() =>
            val thrown = popStack
            checkBool(thrown.toType <:< definitions.ThrowableClass.tpe,
                      "Element on top of stack should implement 'Throwable': " + thrown);
            pushStack(SCALA_NOTHING)

          case DROP(kind) =>
            checkType(popStack, kind)

          case DUP(kind) =>
            val top = popStack
            checkType(top, kind)
            pushStack(top)
            pushStack(top)

          case MONITOR_ENTER() =>
            checkBool(popStack.isReferenceType, "MONITOR_ENTER on non-reference type")

          case MONITOR_EXIT() =>
            checkBool(popStack.isReferenceType, "MONITOR_EXIT on non-reference type")

          case BOX(kind) =>
            checkType(popStack, kind)
            pushStack(icodes.ObjectReference)

          case UNBOX(kind) =>
            popStack
            pushStack(kind)

          case LOAD_EXCEPTION() =>
            clearStack()
            pushStack(ThrowableReference)

          case SCOPE_ENTER(_) | SCOPE_EXIT(_) =>
            ()

          case _ =>
            abort("Unknown instruction: " + instr)
        }
      }
      stack
    }

    //////////////// Error reporting /////////////////////////

    def error(msg: String) {
      Console.println("Error in " + method + ", block: " + basicBlock.label)
      printLastInstructions(8)

      Checkers.this.global.error("ICode checker: " + method + ": " + msg)
    }

    /** Prints the last n instructions. */
    def printLastInstructions(n: Int) {
      val buf = basicBlock.reverse dropWhile (_ != instruction) take n reverse;
      Console.println("Last " + buf.size + " instructions: ")
      buf foreach (Console println _)
      Console.println("at: " + buf.head.pos)
    }

    def error(msg: String, stack: TypeStack) {
      error(msg + "\n type stack: " + stack)
    }

    //////////////////// Checking /////////////////////////////

    /** Return true if <code>k1</code> is a subtype of any of the following
     *  types.
     */
    def isOneOf(k1: TypeKind, kinds: TypeKind*) = kinds exists (k1 <:< _)
  }
}
