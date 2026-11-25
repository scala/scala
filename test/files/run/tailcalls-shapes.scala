import annotation.tailrec

// Each recursive call here must become a jump.
// A match on an integer literal is a switch, other matches have case and `matchEnd` labels.
class P {
  @tailrec final def block(n: Int): Int = { val m = n - 1; if (m < 0) 0 else { { block(m) } } }
  @tailrec final def typed(n: Int): Int = if (n == 0) 0 else (typed(n - 1): Int)
  @tailrec final def annotated(n: Int): Int = if (n == 0) 0 else (annotated(n - 1): @unchecked)
  @tailrec final def typedMatch(n: Int): Int = ((n match { case 0 => 0; case _ => typedMatch(n - 1) }): Int)
  @tailrec final def typedMatchP(n: Int): Int = ((Option(n) match { case Some(0) => 0; case _ => typedMatchP(n - 1) }): Int)
  @tailrec final def ret(n: Int): Int = { if (n == 0) return 0; return ret(n - 1) }
  @tailrec final def retMatch(n: Int): Int = return n match { case 0 => 0; case _ => retMatch(n - 1) }
  @tailrec final def retMatchP(n: Int): Int = return Option(n) match { case Some(0) => 0; case _ => retMatchP(n - 1) }
  @tailrec final def retIfPat(n: Int): Int = return if (n <= 0) 0 else Option(n) match { case Some(1) => retIfPat(0); case _ => retIfPat(n - 1) }
  @tailrec final def typedIfPat(n: Int): Int = (if (n <= 0) 0 else Option(n) match { case Some(1) => typedIfPat(0); case _ => typedIfPat(n - 1) }): Int
  @tailrec final def thenPat(n: Int): Int = if (n > 0) Option(n) match { case Some(1) => thenPat(0); case _ => thenPat(n - 1) } else 0
  @tailrec final def switchPat(n: Int): Int = n match { case 0 => 0; case 1 => Option(n) match { case Some(_) => switchPat(0); case _ => 9 }; case _ => Option(n) match { case Some(_) => switchPat(n - 1); case _ => 9 } }
  @tailrec final def retIf(n: Int): Int = return if (n == 0) 0 else retIf(n - 1)
  @tailrec final def matchThen(n: Int): Int = if (n > 0) n match { case 5 => matchThen(4); case _ => matchThen(n - 1) } else 0
  @tailrec final def matchElse(n: Int): Int = if (n <= 0) 0 else n match { case 5 => matchElse(4); case _ => matchElse(n - 1) }
  @tailrec final def matchBlock(n: Int): Int = { val m = n; m match { case 0 => 0; case _ => matchBlock(m - 1) } }
  @tailrec final def nested(n: Int, o: Any): Int = o match {
    case Some(x) => x match { case i: Int => if (n == 0) 0 else nested(n - 1, Some("")); case s: String => nested(n - 1 max 0, if (n == 0) None else Some(1)) }
    case None => 0
  }
  @tailrec final def nestedIf(n: Int, o: Option[Int]): Int = o match {
    case Some(x) if x > 0 => if (n > 0) o match { case Some(_) => nestedIf(n - 1, o); case _ => 1 } else 0
    case _ => -1
  }
  @tailrec final def guard(n: Int): Int = n match { case _ if n > 10 => guard(n - 1); case _ if n > 0 => guard(n - 1); case _ => 0 }
  @tailrec final def switch(n: Int): Int = n match { case 0 => 0; case 1 => switch(0); case 2 => switch(1); case _ => switch(n - 1) }
  @tailrec final def strSwitch(n: Int, s: String): Int = s match { case "a" => strSwitch(n, "b"); case "b" => if (n == 0) 0 else strSwitch(n - 1, "a"); case _ => 1 }
  @tailrec final def alt(n: Int): Int = n match { case 0 | -1 => 0; case _ => alt(n - 1) }
  @tailrec final def list(l: List[Int], acc: Int): Int = l match { case x :: (y :: t) => list(y :: t, acc + x - x); case x :: Nil => list(Nil, 0); case Nil => acc }
  @tailrec final def and(n: Int): Boolean = n >= 0 && (n == 0 || and(n - 1))
  @tailrec final def andMatch(n: Int): Boolean = n >= 0 && (n match { case 0 => true; case _ => andMatch(n - 1) })
  @tailrec final def andMatchP(n: Int): Boolean = n >= 0 && (Option(n) match { case Some(0) => true; case _ => andMatchP(n - 1) })
  @tailrec final def orMatch(n: Int): Boolean = n < 0 || (n match { case 0 => true; case _ => orMatch(n - 1) })
  @tailrec final def orMatchP(n: Int): Boolean = n < 0 || (Option(n) match { case Some(0) => true; case _ => orMatchP(n - 1) })
  @tailrec final def unit(n: Int): Unit = if (n > 0) unit(n - 1)
  @tailrec final def unitMatch(n: Int): Unit = n match { case 0 => ; case _ => unitMatch(n - 1) }
  @tailrec final def unitMatchP(n: Int): Unit = Option(n) match { case Some(0) => ; case _ => unitMatchP(n - 1) }
  @tailrec final def poly[A](n: Int, a: A): A = if (n == 0) a else poly[A](n - 1, a)
  @tailrec final def polyOther[A](n: Int, a: A): Int = if (n == 0) 0 else polyOther(n - 1, List(1))
  @tailrec final def boxed[A](n: Int, a: A): Any = if (n == 0) a else boxed[Any](n - 1, a)
  @tailrec final def varargs(n: Int, xs: Int*): Int = if (n == 0) xs.size - xs.size else varargs(n - 1, xs: _*)
  @tailrec final def byName(n: Int, x: => Int): Int = if (n == 0) x else byName(n - 1, 0)
  @tailrec final def curried(n: Int)(m: Int): Int = if (n == 0) m else curried(n - 1)(m)
  @tailrec final def named(a: Int, b: Int): Int = if (a == 0) b else named(b = b, a = a - 1)
  @tailrec final def deflt(n: Int, d: Int = 0): Int = if (n == 0) d else deflt(n - 1)
  @tailrec final def impl(n: Int)(implicit o: Ordering[Int]): Int = if (n == 0) 0 else impl(n - 1)
  @tailrec final def swap(n: Int, a: Int, b: Int): Int = if (n == 0) b - b else swap(n - 1, b, a)
  @tailrec final def catches(n: Int): Int = if (n == 0) 0 else { try throw new Exception catch { case _: RuntimeException => 1; case _: Exception => catches(n - 1) } }
  @tailrec final def afterLoop(n: Int): Int = { var i = 0; do i += 1 while (i < 2); if (n == 0) 0 else afterLoop(n - 1) }
  @tailrec final def afterTry(n: Int): Int = { try () finally (); if (n == 0) 0 else afterTry(n - 1) }
  @tailrec final def vari(n: Int): Nothing = if (n == 0) throw new IllegalStateException else vari(n - 1)
  def local(k: Int): Int = { var log = 0; @tailrec def go(n: Int): Int = if (n == 0) log else { log = 0; go(n - 1) }; go(k) }
  @tailrec private[this] def priv(n: Int): Int = if (n == 0) 0 else this.priv(n - 1)
  def callPriv(n: Int) = priv(n)
}
trait T { @tailrec final def inTrait(n: Int): Int = if (n == 0) 0 else inTrait(n - 1); @tailrec private def p(n: Int): Int = if (n == 0) 0 else p(n - 1); def callP(n: Int) = p(n) }
class V(val u: Int) extends AnyVal { @tailrec final def vc(n: Int): Int = if (n == 0) 0 else new V(u).vc(n - 1) }
class Sp[@specialized(Int) A](a: A) { @tailrec final def s(n: Int): Int = if (n == 0) 0 else s(n - 1) }
class Node(val v: Int, val next: Node) { @tailrec final def sum(acc: Int): Int = if (next == null) acc + v else next.sum(acc + v) }
object O { @tailrec def viaName(n: Int): Int = if (n == 0) 0 else O.viaName(n - 1) }

// Recursive calls in other positions, with an observable result
class N(val id: Int) {
  final def tryPat(n: Int): Int = try { Option(n) match { case Some(0) => throw new Exception; case _ => tryPat(n - 1) } } catch { case _: Exception if n > 0 => n }
  final def tryReturnPat(n: Int): Int = try { return Option(n) match { case Some(0) => throw new Exception; case _ => tryReturnPat(n - 1) } } catch { case _: Exception if n > 0 => n }
  final def finPat(n: Int): Int = { try { Option(n) match { case Some(0) => 0; case _ => finPat(n - 1) } } finally note("p" + n) }
  final def mixedPat(n: Int): Int = Option(n) match { case Some(0) => 0; case Some(x) if x % 2 == 0 => mixedPat(n - 1); case _ => 1 + mixedPat(n - 1) }

  var log = List.empty[String]
  def note(s: String): Unit = log ::= s
  final def tryBody(n: Int): Int = try { if (n == 0) throw new Exception; tryBody(n - 1) } catch { case _: Exception if n > 0 => n }
  final def tryReturn(n: Int): Int = try { if (n > 0) return tryReturn(n - 1); throw new Exception } catch { case _: Exception if n > 0 => n }
  final def fin(n: Int): Int = { try { if (n > 0) return fin(n - 1) } finally note("f" + n); 0 }
  final def finBody(n: Int): Unit = try note("b" + n) finally { if (n > 0) finBody(n - 1) }
  final def catchFin(n: Int): Int = try { if (n == 0) 0 else throw new Exception } catch { case _: Exception => catchFin(n - 1) } finally note("c" + n)
  final def and(n: Int): Boolean = n == 0 || (and(n - 1) && n < 2)
  final def recv(n: Int): N = if (n == 0) new N(id + 1) else recv(n - 1).recv(0)
  final def qual(n: Int): String = if (n == 0) "a" else qual(n - 1).concat("b")
  final def mixedMatch(n: Int): Int = n match { case 0 => 0; case _ if n % 2 == 0 => mixedMatch(n - 1); case _ => 1 + mixedMatch(n - 1) }
  final def mixedSync(n: Int): Int = synchronized { if (n == 0) 0 else 1 + mixedSync(n - 1) }
  final def arr(n: Int): Array[Any] = if (n == 0) Array("x") else Array[Any](arr(n - 1))
  final def capture(n: Int, fs: List[() => Int]): List[Int] = if (n == 0) fs.map(_()) else capture(n - 1, (() => n) :: fs)
  final def args(a: Int, b: Int, n: Int): (Int, Int) = if (n == 0) (a, b) else args(b, a + b, n - 1)
}
object Test {
  def main(a: Array[String]): Unit = {
    val k = 100000; val p = new P
    implicit class Ck(x: Any) { def is(y: Any) = assert(x == y, s"$x != $y") }
    val loops = List[Int => Any](
      p.block(_), p.typed(_), p.annotated(_), p.typedMatch(_), p.typedMatchP(_), p.ret(_),
      p.retMatch(_), p.retMatchP(_), p.retIf(_), p.retIfPat(_), p.typedIfPat(_), p.thenPat(_),
      p.switchPat(_), p.matchThen(_), p.matchElse(_), p.matchBlock(_), p.nested(_, Some(1)),
      p.nestedIf(_, Some(1)), p.guard(_), p.switch(_), p.strSwitch(_, "a"), p.alt(_), n =>
      p.list(List.fill(n)(1), 0), p.poly(_, 0), p.polyOther(_, ""), p.boxed(_, 0), p.varargs(_, 1,
      2), p.byName(_, 0), p.curried(_)(0), p.named(_, 0), p.deflt(_), p.impl(_), p.swap(_, 1, 2),
      p.catches(_), p.afterLoop(_), p.afterTry(_), p.local(_), p.callPriv(_), (new T {}).inTrait(_),
      (new T {}).callP(_), new V(1).vc(_), new Sp(1).s(_), O.viaName(_)
    )
    for (r <- loops) r(k) is 0
    p.and(k) is true; p.andMatch(k) is true; p.andMatchP(k) is true; p.orMatch(k) is true; p.orMatchP(k) is true; p.unit(k); p.unitMatch(k); p.unitMatchP(k)
    try p.vari(k) catch { case _: IllegalStateException => }
    (1 to k).foldLeft(null: Node)((n, _) => new Node(1, n)).sum(0) is k
    def n = new N(0)
    n.tryBody(3) is 1; n.tryReturn(3) is 1
    val f = n; f.fin(2); f.log is List("f2", "f1", "f0")
    val b = n; b.finBody(2); b.log is List("b0", "b1", "b2")
    val c = n; c.catchFin(2) is 0; c.log is List("c2", "c1", "c0")
    n.tryPat(3) is 1; n.tryReturnPat(3) is 1; n.mixedPat(4) is 2; val q = n; q.finPat(2) is 0; q.log is List("p2", "p1", "p0")
    n.and(3) is false; n.recv(2).id is 3; n.qual(3) is "abbb"; n.mixedMatch(4) is 2; n.mixedSync(3) is 3
    n.arr(1).apply(0).asInstanceOf[Array[Any]].apply(0) is "x"
    n.capture(3, Nil) is List(1, 2, 3); n.args(0, 1, 5) is ((5, 8))
  }
}
