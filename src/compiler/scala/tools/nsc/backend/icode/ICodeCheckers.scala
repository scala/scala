/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package icode

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

abstract class ICodeCheckers {
  val global: Global
  import global._

  /** <p>
   *    This class performs a set of checks similar to what the bytecode
   *    verifier does. For each basic block, it checks that:
   *  </p>
   *  <ul>
   *    <li>
   *      for primitive operations: the type and number of operands match
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
   *  @todo Better checks for `MONITOR_ENTER/EXIT`
   *        Better checks for local var initializations
   *
   *  @todo Iulian says: I think there's some outdated logic in the checker.
   * The issue with exception handlers being special for least upper
   * bounds pointed out some refactoring in the lattice class. Maybe
   * a worthwhile refactoring would be to make the checker use the
   * DataFlowAnalysis class, and use the lattice trait. In the
   * implementation of LUB, there's a flag telling if one of the
   * successors is 'exceptional'. The inliner is using this mechanism.
   */
  class ICodeChecker {
    import icodes._
    import opcodes._

    var clasz: IClass = _
    var method: IMethod = _
    var code: Code = _

    val in: mutable.Map[BasicBlock, TypeStack]  = perRunCaches.newMap()
    val out: mutable.Map[BasicBlock, TypeStack] = perRunCaches.newMap()
    val emptyStack = new TypeStack() {
      override def toString = "<empty>"
    }

    /** The presence of emptyStack means that path has not yet been checked
     *  (and may not be empty).
     */
    def notChecked(ts: TypeStack) = ts eq emptyStack
    def initMaps(bs: Seq[BasicBlock]): Unit = {
      in.clear()
      out.clear()
      bs foreach { b =>
        in(b) = emptyStack
        out(b) = emptyStack
      }
    }

    /** A wrapper to route log messages to debug output also.
     */
    def logChecker(msg: String) = {
      log(msg)
      checkerDebug(msg)
    }

    def checkICodes(): Unit = {
      if (settings.verbose)
      println("[[consistency check at the beginning of phase " + globalPhase.name + "]]")
      classes.values foreach check
    }

    private def posStr(p: Position) =
      if (p.isDefined) p.line.toString else "<??>"

    private def indent(s: String, prefix: String): String = {
      val lines = s split "\\n"
      lines map (prefix + _) mkString "\n"
    }

    /** Only called when m1 < m2, so already known that (m1 ne m2).
     */
    private def isConflict(m1: IMember, m2: IMember, canOverload: Boolean) = (
      (m1.symbol.name == m2.symbol.name) &&
      (!canOverload || (m1.symbol.tpe =:= m2.symbol.tpe))
    )

    def check(cls: IClass) {
      logChecker("\n<<-- Checking class " + cls + " -->>")
      clasz = cls

      for (f1 <- cls.fields ; f2 <- cls.fields ; if f1 < f2)
        if (isConflict(f1, f2, canOverload = false))
          icodeError("Repetitive field name: " + f1.symbol.fullName)

      for (m1 <- cls.methods ; m2 <- cls.methods ; if m1 < m2)
        if (isConflict(m1, m2, canOverload = true))
          icodeError("Repetitive method: " + m1.symbol.fullName)

      clasz.methods foreach check
    }

    def check(m: IMethod) {
      logChecker("\n<< Checking method " + m.symbol.name + " >>")
      method = m
      if (!m.isAbstractMethod)
        check(m.code)
    }

    def check(c: Code) {
      val worklist = new ListBuffer[BasicBlock]
      def append(elems: List[BasicBlock]) =
        worklist ++= (elems filterNot (worklist contains _))

      code = c
      worklist += c.startBlock
      initMaps(c.blocks)

      while (worklist.nonEmpty) {
        val block  = worklist remove 0
        val output = check(block, in(block))
        if (output != out(block) || notChecked(out(block))) {
          if (block.successors.nonEmpty)
            logChecker("** Output change for %s: %s -> %s".format(block, out(block), output))

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

      def hasNothingType(s: TypeStack) = s.nonEmpty && (s.head == NothingReference)

      /* XXX workaround #1: one stack empty, the other has BoxedUnit.
       * One example where this arises is:
       *
       * def f(b: Boolean): Unit = synchronized { if (b) () }
       */
      def allUnits(s: TypeStack)   = s.types forall (_ == BoxedUnitReference)

      def ifAthenB[T](f: T => Boolean): PartialFunction[(T, T), T] = {
        case (x1, x2) if f(x1)  => x2
        case (x1, x2) if f(x2)  => x1
      }

      /* XXX workaround #2: different stacks heading into an exception
       * handler which will clear them anyway.  Examples where it arises:
       *
       * var bippy: Int = synchronized { if (b) 5 else 10 }
       */
      def isHandlerBlock() = bl.exceptionHandlerStart

      def meet2(s1: TypeStack, s2: TypeStack): TypeStack = {
        def workaround(msg: String) = {
          checkerDebug(msg + ": " + method + " at block " + bl)
          checkerDebug("  s1: " + s1)
          checkerDebug("  s2: " + s2)
          new TypeStack()
        }
        def incompatibleString = (
          "Incompatible stacks: " + s1 + " and " + s2 + " in " + method + " at entry to block " + bl.label + ":\n" +
          indent(bl.predContents, "// ") +
          indent(bl.succContents, "// ") +
          indent(bl.blockContents, "// ")
        )

        val f: ((TypeStack, TypeStack)) => TypeStack = {
          ifAthenB(notChecked) orElse ifAthenB(hasNothingType) orElse {
            case (s1: TypeStack, s2: TypeStack) =>
              if (s1.length != s2.length) {
                if (allUnits(s1) && allUnits(s2))
                  workaround("Ignoring mismatched boxed units")
                else if (isHandlerBlock())
                  workaround("Ignoring mismatched stacks entering exception handler")
                else
                  throw new CheckerException(incompatibleString)
              }
              else {
                val newStack: TypeStack = try {
                    new TypeStack((s1.types, s2.types).zipped map lub)
                } catch {
                  case t: Exception =>
                    checkerDebug(t.toString + ": " + s1.types.toString + " vs " + s2.types.toString)
                    new TypeStack(s1.types)
                }
                if (newStack.isEmpty || s1.types == s2.types) ()  // not interesting to report
                else checkerDebug("Checker created new stack:\n  (%s, %s) => %s".format(s1, s2, newStack))

                newStack
              }
          }
        }

        f((s1, s2))
      }

      if (preds.nonEmpty) {
        in(bl) = (preds map out.apply) reduceLeft meet2
        log("Input changed for block: " + bl +" to: " + in(bl))
      }
    }

    private var instruction: Instruction = null
    private var basicBlock: BasicBlock = null
    private var stringConcatDepth = 0
    private def stringConcatIndent() = "  " * stringConcatDepth
    private def currentInstrString: String = {
      val (indent, str) = this.instruction match {
        case CALL_PRIMITIVE(StartConcat)      =>
          val x = stringConcatIndent()
          stringConcatDepth += 1
          (x, "concat(")
        case CALL_PRIMITIVE(EndConcat)        =>
          if (stringConcatDepth > 0) {
            stringConcatDepth -= 1
            (stringConcatIndent(), ") // end concat")
          }
          else ("", "")
        case _ =>
          (stringConcatIndent(), this.instruction match {
            case CALL_PRIMITIVE(StringConcat(el)) => "..."
            case null                             => "null"
            case cm @ CALL_METHOD(_, _)           => if (clasz.symbol == cm.hostClass) cm.toShortString else cm.toString
            case x                                => x.toString
          })
      }
      indent + str
    }
    /** A couple closure creators to reduce noise in the output: when multiple
     *  items are pushed or popped, this lets us print something short and sensible
     *  for those beyond the first.
     */
    def mkInstrPrinter(f: Int => String): () => String = {
      var counter = -1
      val indent = stringConcatIndent()
      () => {
        counter += 1
        if (counter == 0) currentInstrString
        else indent + f(counter)
      }
    }
    def defaultInstrPrinter: () => String = mkInstrPrinter(_ => "\"\"\"")

    /**
     * Check the basic block to be type correct and return the
     * produced type stack.
     */
    def check(b: BasicBlock, initial: TypeStack): TypeStack = {
      this.basicBlock = b

      logChecker({
        val prefix = "** Checking " + b.fullString

        if (initial.isEmpty) prefix
        else prefix + " with initial stack " + initial.types.mkString("[", ", ", "]")
      })

      val stack = new TypeStack(initial)
      def checkStack(len: Int) {
        if (stack.length < len)
          ICodeChecker.this.icodeError("Expected at least " + len + " elements on the stack", stack)
      }

      def sizeString(push: Boolean) = {
        val arrow = if (push) "-> " else "<- "
        val sp    = "   " * stack.length

        sp + stack.length + arrow
      }
      def printStackString(isPush: Boolean, value: TypeKind, instrString: String) = {
        val pushString = if (isPush) "+" else "-"
        val posString  = posStr(this.instruction.pos)

        checkerDebug("%-70s %-4s %s %s".format(sizeString(isPush) + value, posString, pushString, instrString))
      }
      def _popStack: TypeKind = {
        if (stack.isEmpty) {
          icodeError("Popped empty stack in " + b.fullString + ", throwing a Unit")
          return UNIT
        }
        stack.pop
      }
      def popStackN(num: Int, instrFn: () => String = defaultInstrPrinter) = {
        List.range(0, num) map { _ =>
          val res = _popStack
          printStackString(isPush = false, res, instrFn())
          res
        }
      }
      def pushStackN(xs: Seq[TypeKind], instrFn: () => String) = {
        xs foreach { x =>
          stack push x
          printStackString(isPush = true, x, instrFn())
        }
      }

      def popStack     = { checkStack(1) ; (popStackN(1): @unchecked) match { case List(x) => x } }
      def popStack2    = { checkStack(2) ; (popStackN(2): @unchecked) match { case List(x, y) => (x, y) } }
      def popStack3    = { checkStack(3) ; (popStackN(3): @unchecked) match { case List(x, y, z) => (x, y, z) } }

      /* Called by faux instruction LOAD_EXCEPTION to wipe out the stack. */
      def clearStack() = {
        if (stack.nonEmpty)
          logChecker("Wiping out the " + stack.length + " element stack for exception handler: " + stack)

        1 to stack.length foreach (_ => popStack)
      }

      def pushStack(xs: TypeKind*): Unit = {
        pushStackN(xs filterNot (_ == UNIT), defaultInstrPrinter)
      }

      def typeError(k1: TypeKind, k2: TypeKind) {
        icodeError("\n  expected: " + k1 + "\n     found: " + k2)
      }
      def isSubtype(k1: TypeKind, k2: TypeKind) = (k1 isAssignabledTo k2) || {
        import platform.isMaybeBoxed

        (k1, k2) match {
          case (REFERENCE(_), REFERENCE(_)) if k1.isInterfaceType || k2.isInterfaceType =>
            logChecker("Considering %s <:< %s because at least one is an interface".format(k1, k2))
            true
          case (REFERENCE(cls1), REFERENCE(cls2)) if isMaybeBoxed(cls1) || isMaybeBoxed(cls2) =>
            logChecker("Considering %s <:< %s because at least one might be a boxed primitive".format(cls1, cls2))
            true
          case _ =>
            false
        }
      }

      def subtypeTest(k1: TypeKind, k2: TypeKind): Unit =
        if (isSubtype(k1, k2)) ()
        else typeError(k2, k1)

      for (instr <- b) {
        this.instruction = instr

        def checkLocal(local: Local) {
          if ((method lookupLocal local.sym.name).isEmpty)
            icodeError(s" $local is not defined in method $method")
        }
        def checkField(obj: TypeKind, field: Symbol): Unit = obj match {
          case REFERENCE(sym) =>
            if (sym.info.member(field.name) == NoSymbol)
              icodeError(" " + field + " is not defined in class " + clasz)
          case _ =>
            icodeError(" expected reference type, but " + obj + " found")
        }

        /* Checks that tpe is a subtype of one of the allowed types */
        def checkType(tpe: TypeKind, allowed: TypeKind*) = (
          if (allowed exists (k => isSubtype(tpe, k))) ()
          else icodeError(tpe + " is not one of: " + allowed.mkString("{ ", ", ", " }"))
        )
        def checkNumeric(tpe: TypeKind) =
          checkType(tpe, BYTE, CHAR, SHORT, INT, LONG, FLOAT, DOUBLE)

        /* Checks that the 2 topmost elements on stack are of the kind TypeKind. */
        def checkBinop(kind: TypeKind) {
          val (a, b) = popStack2
          checkType(a, kind)
          checkType(b, kind)
        }

        /* Check that arguments on the stack match method params. */
        def checkMethodArgs(method: Symbol) {
          val params = method.info.paramTypes
          checkStack(params.length)
          (
            popStackN(params.length, mkInstrPrinter(num => "<arg" + num + ">")),
            params.reverse map toTypeKind).zipped foreach ((x, y) => checkType(x, y)
          )
        }

        /* Checks that the object passed as receiver has a method
         * `method` and that it is callable from the current method.
         */
        def checkMethod(receiver: TypeKind, method: Symbol) =
          receiver match {
            case REFERENCE(sym) =>
              checkBool(sym.info.member(method.name) != NoSymbol,
                        "Method " + method + " does not exist in " + sym.fullName)
              if (method.isPrivate)
                checkBool(method.owner == clasz.symbol,
                          "Cannot call private method of " + method.owner.fullName
                          + " from " + clasz.symbol.fullName)
              else if (method.isProtected) {
                val isProtectedOK = (
                  (clasz.symbol isSubClass method.owner) ||
                  (clasz.symbol.typeOfThis.typeSymbol isSubClass method.owner)  // see pos/bug780.scala
                )

                checkBool(isProtectedOK,
                          "Cannot call protected method of " + method.owner.fullName
                          + " from " + clasz.symbol.fullName)
              }

            case ARRAY(_) =>
              checkBool(receiver.toType.member(method.name) != NoSymbol,
                        "Method " + method + " does not exist in " + receiver)

            case t =>
              icodeError("Not a reference type: " + t)
          }

        def checkBool(cond: Boolean, msg: String) =
          if (!cond) icodeError(msg)

        if (settings.debug) {
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
                icodeError(" expected an INT and an array reference, but " +
                    a + ", " + b + " found")
            }

         case LOAD_LOCAL(local) =>
           checkLocal(local)
           pushStack(local.kind)

         case LOAD_FIELD(field, isStatic) =>
           // the symbol's owner should contain its field, but
           // this is already checked by the type checker, no need
           // to redo that here
           if (isStatic) ()
           else checkField(popStack, field)

           pushStack(toTypeKind(field.tpe))

         case LOAD_MODULE(module) =>
           checkBool((module.isModule || module.isModuleClass),
                     "Expected module: " + module + " flags: " + module.flagString)
           pushStack(toTypeKind(module.tpe))

          case STORE_THIS(kind) =>
           val actualType = popStack
           if (actualType.isReferenceType) subtypeTest(actualType, kind)
           else icodeError("Expected this reference but found: " + actualType)

         case STORE_ARRAY_ITEM(kind) =>
           popStack3 match {
             case (k, INT, ARRAY(elem)) =>
               subtypeTest(k, kind)
               subtypeTest(k, elem)
             case (a, b, c) =>
                icodeError(" expected and array reference, and int and " + kind +
                      " but " + a + ", " + b + ", " + c + " found")
           }

         case STORE_LOCAL(local) =>
           checkLocal(local)
           val actualType = popStack
           if (local.kind != NullReference)
            subtypeTest(actualType, local.kind)

         case STORE_FIELD(field, true) =>     // static
           val fieldType = toTypeKind(field.tpe)
           val actualType = popStack
           subtypeTest(actualType, fieldType)

         case STORE_FIELD(field, false) =>    // not static
           val (value, obj) = popStack2
           checkField(obj, field)
           val fieldType = toTypeKind(field.tpe)
           if (fieldType == NullReference) ()
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
                 case arr         => icodeError(" array reference expected, but " + arr + " found")
               }
               pushStack(INT)

             case StartConcat =>
               pushStack(ConcatClass)

             case EndConcat =>
               checkType(popStack, ConcatClass)
               pushStack(StringReference)

             case StringConcat(el) =>
               checkType(popStack, el)
               checkType(popStack, ConcatClass)
               pushStack(ConcatClass)
           }

         case CALL_METHOD(method, style) =>
           // PP to ID: I moved the if (!method.isConstructor) check to cover all
           // the styles to address checker failure.  Can you confirm if the change
           // was correct? If I remember right it's a matter of whether some brand
           // of supercall should leave a value on the stack, and I know there is some
           // trickery performed elsewhere regarding this.
           val paramCount = method.info.paramTypes.length match {
             case x if style.hasInstance  => x + 1
             case x                       => x
           }
           if (style == Static(onInstance = true))
             checkBool(method.isPrivate || method.isConstructor, "Static call to non-private method.")

          checkStack(paramCount)
          checkMethodArgs(method)
          if (style.hasInstance)
            checkMethod(popStack, method)
          if (!method.isConstructor)
            pushStack(toTypeKind(method.info.resultType))

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
            else checkBool(!top.isValueType, "" + kind + " is a reference type, but " + top + " is not")

          case THROW(clasz) =>
            checkType(popStack, toTypeKind(clasz.tpe))
            pushStack(NothingReference)

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
            pushStack(REFERENCE(definitions.boxedClass(kind.toType.typeSymbol)))

          case UNBOX(kind) =>
            popStack
            pushStack(kind)

          case LOAD_EXCEPTION(clasz) =>
            clearStack()
            pushStack(REFERENCE(clasz))

          case SCOPE_ENTER(_) | SCOPE_EXIT(_) =>
            ()

          case _ =>
            abort("Unknown instruction: " + instr)
        }
      }
      stack
    }

    //////////////// Error reporting /////////////////////////

    def icodeError(msg: String) {
      ICodeCheckers.this.global.warning(
        "!! ICode checker fatality in " + method +
        "\n  at: " + basicBlock.fullString +
        "\n  error message: " + msg
      )
    }

    def icodeError(msg: String, stack: TypeStack) {
      icodeError(msg + "\n type stack: " + stack)
    }
  }
}
