package scala.concurrent;

/**
* Library for using Pi-calculus concurrent primitives in Scala.
*/
object pilib with Monitor {

  /////////////////////////// SPAWN //////////////////////////////

  /**
  * Run several processes in parallel using the following syntax:
  * spawn < p_1 | ... | p_n >
  */
  trait Spawn {
    def <(def p: unit): Spawn;
    def |(def p: unit): Spawn;
    def > : unit;
  }
  val spawn = new Spawn {
  //  object spawn extends Spawn { BUG !
    def <(def p: unit): Spawn = { ops.spawn(p); this }
    def |(def p: unit): Spawn = { ops.spawn(p); this }
    def > : unit = ()
  }

  //////////////////////// GUARDED PROCESSES /////////////////////////

  type Name = AnyRef;

  /** A guarded process.
  * @param a         channel
  * @param direction input (true) or output (false)
  * @param v         transmitted value
  * @param c         continuation
  */
  case class GP(a: Name, direction: boolean, v: Any, c: Any => unit);

  ////////////////////////// CHANNELS //////////////////////////////

  /**
  * Name on which one can emit, receive or that can be emitted or received
  * during a communication.
    */
  class Chan[a] with Function1[a, Product[a]] {

    /** Creates an input guarded process. */
    def input(c: a => unit) =
      GP(this, true, (), v => c(v.asInstanceOf[a]));

    /** Creates an input guarded process. */
    def output(v: a, c: () => unit) =
      GP(this, false, v, x => c());

    /** Blocking read. */
    def read = {
      var res: a = _;
      choice ( input(x => res = x) );
      res
    }

    /** Blocking write. */
    def write(x: a) =
      choice ( output(x, () => ()) );

    /** Syntactic sugar for input. */
    def *(f: a => unit) =
      input(f);

    /** Syntactic sugar for output. */
    def apply(v: a) =
      new Product(this, v);
  }

  class Product[a](c: Chan[a], v: a) {
    def *(def f: unit) = c.output(v, () => f);
  }

  //////////////////// SUM OF GUARDED PROCESSES //////////////////////

  case class Sum(gs: List[GP]) with Monitor {

    /** Continuation of the sum. */
    var cont: () => unit = _;

    var initialized = false;

    /**
    * Block if not initialized otherwise continue with the
    * continuation.
    */
    def continue = synchronized {
      if (!initialized) wait();
      cont()
    }

    /** Set the values of parameters and awake the sleeping sum. */
    def set(f: () => unit) = synchronized {
      cont = f;
      initialized = true;
      notify()
    }
  }

  /////////////////////////// COMMUNICATION  //////////////////////////

  private var sums: List[Sum] = Nil;

  /** Test if two lists of guarded processes can communicate. */
  private def matches(gs1: List[GP], gs2: List[GP]):
  Option[Tuple4[Any, Any => unit, Any, Any => unit]] =
    Pair(gs1, gs2) match {
      case Pair(Nil, _) => None
      case Pair(_, Nil) => None
      case Pair(GP(a1, d1, v1, c1) :: rest1, GP(a2, d2, v2, c2) :: rest2) =>
	if (a1 == a2 && d1 == !d2)
	  Some(Tuple4(v1, c1, v2, c2))
	else matches(gs1, rest2) match {
	  case None => matches(rest1, gs2)
	  case Some(t) => Some(t)
	}
    }

  /**
  * Test if the given sum can react with one of the pending sums.
  * If yes then do the reaction otherwise append the sum at the end
  * of the pending sums.
  */
  private def compare(s1: Sum, ss: List[Sum]): List[Sum] =
    ss match {
      case Nil => ss ::: List(s1)
      case s2 :: rest => matches(s1.gs, s2.gs) match {
	case None => s2 :: compare(s1, rest)
	case Some(Tuple4(v1, c1, v2, c2)) => {
	  s1.set(() => c1(v2));
	  s2.set(() => c2(v1));
	  rest
	}
      }
    }

  /** Pi-calculus non-deterministic choice. */
  def choice(s: GP*): unit = {
    val sum = Sum(s.asInstanceOf[List[GP]]);
    synchronized { sums = compare(sum, sums) };
    sum.continue
  }


}

/* Former version.

package scala.concurrent;

/**
* Library for using Pi-calculus concurrent primitives in Scala.
*/
object pilib with Monitor {

  /**
  * Run several processes in parallel using the following syntax:
  * spawn < p_1 | ... | p_n >
  */
  trait Spawn {
    def <(def p: unit): Spawn;
    def |(def p: unit): Spawn;
    def > : unit;
  }
  val spawn = new Spawn {
//  object spawn extends Spawn {
    def <(def p: unit): Spawn = { concurrent.ops.spawn(p); this }
    def |(def p: unit): Spawn = { concurrent.ops.spawn(p); this }
    def > : unit = ()
  }

  type Sum = List[GuardedProcess];

  /** List of pending choices. */
  private var sums: List[Sum] = Nil;

  /**
  * Look in the given sum for a branch guarded by an input on the
  * name a.
  */
  private def lookupIn(s: Sum, a: AbstractChan): Option[Any => unit] =
    s match {
      case Nil => None
      case InputGuardedProcess(b, p) :: rest => if (a == b) Some(p) else lookupIn(rest, a)
      case OutputGuardedProcess(_, _, _) :: rest => lookupIn(rest, a)
    };

  /**
  * Look in the given sum for a branch guarded by an output on the
  * name a.
  */
  private def lookupOut(sum: Sum, a: AbstractChan): Option[Pair[Any, () => unit]] =
    sum match {
      case Nil => None
      case OutputGuardedProcess(b, x, q) :: rest => if (a == b) Some(Pair(x, q)) else lookupOut(rest, a)
      case InputGuardedProcess(_, _) :: rest => lookupOut(rest, a)
    };

  /**
  * Check if the two given sums can communicate, returns the parameters of the
  * communication in this case.
  */
  private def canCom(s1: Sum, s2: Sum): Option[Triple[Any, Any => unit, () => unit]] =
    s1 match {
      case Nil => None
      case InputGuardedProcess(a, p) :: rest => lookupOut(s2, a) match {
	case None => canCom(rest, s2)
	case Some(Pair(x, q)) => Some(Triple(x, p, q))
      }
      case OutputGuardedProcess(b, x, q) :: rest => lookupIn(s2, b) match {
	case None => canCom(rest, s2)
	case Some(p) => Some(Triple(x, p, q))
      }
    };

  /**
  * Tries to find in the list of pending sums one which can performs a communication
  * with the given sum. In this case, removes the matching pending sum from the list and
  * does the communication. Otherwise adds the given sum at the end of the pending sums.
  */
  private def communicateWith(s: Sum): unit = {
    def comWith(hd: List[Sum], tl: List[Sum]): List[Sum] =
      tl match {
	case Nil => hd ::: List(s)
	case s1 :: rest => canCom(s, s1) match {
	  case None => comWith(hd ::: List(s1), rest)
	  case Some(Triple(x, p, q)) => {
	    concurrent.ops.spawn(p(x));
	    concurrent.ops.spawn(q());
	    hd ::: rest
	  }
	}
      };

    synchronized {
      sums = comWith(Nil, sums)
    }
  }

  /**
  * Represents a guarded process in a non-deterministic choice.
  */
  abstract class GuardedProcess;

  /**
  * Process guarded by an input prefix in a sum.
  */
  case class InputGuardedProcess(a: AbstractChan, p: Any => unit) extends GuardedProcess;

  /**
  * Process guarded by an output prefix in a sum.
  */
  case class OutputGuardedProcess(b: AbstractChan, x: Any, q: () => unit) extends GuardedProcess;

  abstract class AbstractChan;

  /**
  * Name on which one can emit, receive or that can be emitted or received
  * during a communication.
  */
  class Chan[a] extends AbstractChan with Function1[a, Product[a]] {

    /**
    * Creates an input guarded process.
    */
    def input(p: a => unit): GuardedProcess = InputGuardedProcess(this, (x => p(x.asInstanceOf[a])));

    /**
    * Creates an input guarded process.
    */
    def output(x: a, q: () => unit): GuardedProcess = OutputGuardedProcess(this, x, q);

    /**
    * Blocking read.
    */
    def read: a = {
      val res = new concurrent.SyncVar[a];
      choice ( input(x: a => res.set(x)) );
      res.get
    }

    /**
    * Blocking write.
    */
    def write(x: a): unit = choice ( output(x, () => ()) );

    /**
    * Syntactic sugar.
    */
    def *(f: a => unit) = input(f);

    def apply(x: a): Product[a] =
      new Product[a](this, x);

  }
  /**
  * Syntactic sugar
  */
  class Product[a](c: Chan[a], x: a) {
    def *(def f: unit) = c.output(x, (() => f));
  }

  /**
  * Evaluates a choice (is non blocking).
  */
  def choice(s: GuardedProcess*): unit = {
    val done = new concurrent.Lock;
    done.acquire;
    val new_branches: Sum = (s.asInstanceOf[List[GuardedProcess]]) map {
      case InputGuardedProcess(a, p) => InputGuardedProcess(a, (x => { p(x); done.release }))
      case OutputGuardedProcess(b, x, q) => OutputGuardedProcess(b, x, (() => { q(); done.release }))
    };
    communicateWith(new_branches);
    done.acquire
  }

}

*/
