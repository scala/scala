import annotation.tailrec

// The recursive call in each of these methods is not in tail position.
// A match on an integer literal is a switch, a match on `Option(n)` has case and `matchEnd` labels.
class C(val next: C) {
  var log = 0
  def asAny(a: Any): Any = a

  // the body of a `try`
  @tailrec final def tryBody(n: Int): Int = try { if (n == 0) 0 else tryBody(n - 1) } catch { case _: Exception => 1 }
  @tailrec final def tryMatch(n: Int): Int = try { n match { case 0 => 0; case _ => tryMatch(n - 1) } } catch { case _: Exception => 1 }
  @tailrec final def tryMatchP(n: Int): Int = try { Option(n) match { case Some(0) => 0; case _ => tryMatchP(n - 1) } } catch { case _: Exception => 1 }
  @tailrec final def tryReturn(n: Int): Int = try { if (n > 0) return tryReturn(n - 1); 0 } catch { case _: Exception => 1 }
  @tailrec final def tryReturnPat(n: Int): Int = try { return Option(n) match { case Some(0) => 0; case _ => tryReturnPat(n - 1) } } catch { case _: Exception => 1 }
  @tailrec final def tryFinReturn(n: Int): Int = { try { if (n > 0) return tryFinReturn(n - 1) } finally log += 1; 0 }

  // a finalizer, or a handler when there is one
  @tailrec final def fin(n: Int): Unit = try log += 1 finally fin(n - 1)
  @tailrec final def finMatch(n: Int): Unit = try log += 1 finally { n match { case 0 => ; case _ => finMatch(n - 1) } }
  @tailrec final def finMatchP(n: Int): Unit = try log += 1 finally { Option(n) match { case Some(0) => ; case _ => finMatchP(n - 1) } }
  @tailrec final def catchFinMatch(n: Int): Int = try 1 catch { case _: Exception => n match { case 0 => 0; case _ => catchFinMatch(n - 1) } } finally log += 1
  @tailrec final def catchFinMatchP(n: Int): Int = try 1 catch { case _: Exception => Option(n) match { case Some(0) => 0; case _ => catchFinMatchP(n - 1) } } finally log += 1

  // the left operand of a boolean operator
  @tailrec final def andLhs(n: Int): Boolean = andLhs(n - 1) && n > 0
  @tailrec final def orLhs(n: Int): Boolean = orLhs(n - 1) || n > 0
  @tailrec final def not(n: Int): Boolean = !not(n - 1)
  @tailrec final def lhsMatch(n: Int): Boolean = (n match { case 0 => true; case _ => lhsMatch(n - 1) }) && n > 0
  @tailrec final def lhsMatchP(n: Int): Boolean = (Option(n) match { case Some(0) => true; case _ => lhsMatchP(n - 1) }) && n > 0

  // a qualifier or the receiver of another recursive call
  @tailrec final def qual(n: Int): String = qual(n - 1).trim
  @tailrec final def field(n: Int): C = field(n - 1).next
  @tailrec final def isInst(n: Int): Any = isInst(n - 1).isInstanceOf[String]
  @tailrec final def asInst(n: Int): String = asAny(asInst(n - 1)).asInstanceOf[String]
  @tailrec final def cast(n: Int): AnyRef = cast(n - 1).asInstanceOf[String]
  @tailrec final def recv(n: Int): C = recv(n - 1).recv(n - 2)
  @tailrec final def qualMatch(n: Int): String = (n match { case 0 => ""; case _ => qualMatch(n - 1) }).trim
  @tailrec final def qualMatchP(n: Int): String = (Option(n) match { case Some(0) => ""; case _ => qualMatchP(n - 1) }).trim
  @tailrec final def unbox(n: Int): Any = { val a: Int = unbox(n - 1).asInstanceOf[Int]; a }

  // an argument or an element of an array literal
  @tailrec final def arrayLit(n: Int): Array[Any] = Array[Any](arrayLit(n - 1))
  @tailrec final def newArg(n: Int): AnyRef = new Some(newArg(n - 1))
  @tailrec final def ctorArg(n: Int): C = new C(ctorArg(n - 1))
  @tailrec final def argMatch(n: Int): Int = math.abs(n match { case 0 => 0; case _ => argMatch(n - 1) })
  @tailrec final def argMatchP(n: Int): Int = math.abs(Option(n) match { case Some(0) => 0; case _ => argMatchP(n - 1) })
  @tailrec final def interp(n: Int): String = s"${interp(n - 1)}"
  @tailrec final def concat(n: Int): String = "" + concat(n - 1)
  @tailrec final def widen(n: Int): Any = { def w(i: Int): Long = i; if (n == 0) 0 else w(widen(n - 1).hashCode) }

  // a condition, scrutinee or guard
  @tailrec final def cond(n: Int): Boolean = if (cond(n - 1)) true else false
  @tailrec final def condMatch(n: Int): Int = if (n match { case 0 => true; case _ => condMatch(n - 1) == 0 }) 1 else 2
  @tailrec final def condMatchP(n: Int): Int = if (Option(n) match { case Some(0) => true; case _ => condMatchP(n - 1) == 0 }) 1 else 2
  @tailrec final def scrut(n: Int): Option[Int] = scrut(n - 1) match { case Some(x) if x > 0 => None; case o => o }
  @tailrec final def scrutSwitch(n: Int): Int = scrutSwitch(n - 1) match { case 1 => 2; case 2 => 3; case 3 => 4; case _ => 0 }
  @tailrec final def scrutMatch(n: Int): Int = (n match { case 0 => 0; case _ => scrutMatch(n - 1) }) match { case 1 => 2; case 2 => 3; case 3 => 4; case _ => 0 }
  @tailrec final def scrutMatchP(n: Int): Int = (Option(n) match { case Some(0) => 0; case _ => scrutMatchP(n - 1) }) match { case 1 => 2; case 2 => 3; case 3 => 4; case _ => 0 }
  @tailrec final def guard(n: Int): Int = n match { case _ if guard(n - 1) == 0 => 1; case _ => 2 }

  // a definition or a statement
  @tailrec final def valMatch(n: Int): Int = { val x = n match { case 0 => 0; case _ => valMatch(n - 1) }; x }
  @tailrec final def valMatchP(n: Int): Int = { val x = Option(n) match { case Some(0) => 0; case _ => valMatchP(n - 1) }; x }
  @tailrec final def stmtMatch(n: Int): Unit = { n match { case 0 => ; case _ => stmtMatch(n - 1) }; () }
  @tailrec final def stmtMatchP(n: Int): Unit = { Option(n) match { case Some(0) => ; case _ => stmtMatchP(n - 1) }; () }
  @tailrec final def unitVal(n: Int): Int = { unitVal(n - 1); 0 }
  @tailrec final def unitDiscard(n: Int): Unit = { val u: Unit = unitDiscard(n - 1); u }
  @tailrec final def lzy(n: Int): Int = { lazy val x = lzy(n - 1); x }

  // the condition or body of a loop
  @tailrec final def whileCond(n: Int): Boolean = { while (whileCond(n - 1)) (); false }
  @tailrec final def whileBody(n: Int): Unit = while (n > 0) whileBody(n - 1)
  @tailrec final def doBody(n: Int): Unit = do doBody(n - 1) while (n > 0)
  @tailrec final def doCond(n: Int): Boolean = { do () while (doCond(n - 1)); true }
  @tailrec final def whileMatch(n: Int): Unit = while (n > 0) n match { case 0 => ; case _ => whileMatch(n - 1) }
  @tailrec final def whileMatchP(n: Int): Unit = while (n > 0) Option(n) match { case Some(0) => ; case _ => whileMatchP(n - 1) }

  // a closure, by-name argument or local class
  @tailrec final def closure(n: Int): () => Int = () => closure(n - 1)()
  @tailrec final def byName(n: Int): Int = Option(n).getOrElse(byName(n - 1))
  @tailrec final def local(n: Int): Int = { class L { def m = local(n - 1) }; new L().m }
}
