//############################################################################
// Programmation IV - 2002 - Week 08
//############################################################################

import List._;

object M0 {

  var x: String = "abc";
  var count = 111;

  def test = {
    Console.println("x     = " + x);
    Console.println("count = " + count);
    x = "hello";
    count = count + 1;
    Console.println("x     = " + x);
    Console.println("count = " + count);
    Console.println;
  }
}

//############################################################################

object M1 {

  class BankAccount() {
    private var balance = 0;
    def deposit(amount: Int): Unit =
      if (amount > 0) balance = balance + amount;

    def withdraw(amount: Int): Int =
      if (0 < amount && amount <= balance) {
        balance = balance - amount;
        balance
      } else sys.error("insufficient funds");
  }

  def test0 = {
    val account = new BankAccount();
    Console.print("account deposit  50 -> ");
    Console.println((account deposit  50).toString()); // !!! .toString
    Console.print("account withdraw 20 -> ");
    Console.println(account withdraw 20);
    Console.print("account withdraw 20 -> ");
    Console.println(account withdraw 20);
    Console.print("account withdraw 15 -> ");
    Console.println;
  }

  def test1 = {
    val x = new BankAccount();
    val y = new BankAccount();
    Console.print("x deposit  30 -> ");
    Console.println((x deposit  30).toString()); // !!! .toString
    Console.print("y withdraw 20 -> ");
    Console.println;
  }

  def test2 = {
    val x = new BankAccount();
    val y = new BankAccount();
    Console.print("x deposit  30 -> ");
    Console.println((x deposit  30).toString()); // !!! .toString
    Console.print("x withdraw 20 -> ");
    Console.println(x withdraw 20);
  }

  def test3 = {
    val x = new BankAccount();
    val y = x;
    Console.print("x deposit  30 -> ");
    Console.println((x deposit  30).toString()); // !!! .toString
    Console.print("y withdraw 20 -> ");
    Console.println(y withdraw 20);
  }

  def test = {
    test0; Console.println;
    test1; Console.println;
    test2; Console.println;
    test3; Console.println;
  }
}


//############################################################################

object M2 {

  def While(condition: => Boolean)(command: => Unit): Unit =
    if (condition) {
      command; While(condition)(command)
    } else {
    }

  def power (x: Double, exp: Int): Double = {
    var r = 1.0;
    var i = exp;
    While (i > 0) { r = r * x; i = i - 1 }
    r
  }

  def test = {
    Console.println("2^0 = " + power(2,0));
    Console.println("2^1 = " + power(2,1));
    Console.println("2^2 = " + power(2,2));
    Console.println("2^3 = " + power(2,3));
    Console.println;
  }
}

//############################################################################

object M3 {

  def power (x: Double, exp: Int): Double = {
    var r = 1.0;
    var i = exp;
    while (i > 0) { r = r * x; i = i - 1 }
    r
  }

  def test = {
    Console.println("2^0 = " + power(2,0));
    Console.println("2^1 = " + power(2,1));
    Console.println("2^2 = " + power(2,2));
    Console.println("2^3 = " + power(2,3));
    Console.println;
  }
}

//############################################################################

object M4 {

  def test = {
    for (i <- range(1, 4)) { Console.print(i + " ") };
    Console.println;
    Console.println(for (i <- range(1, 4)) yield i);
    Console.println;
  }
}

//############################################################################

object M5 {

  type Action = () => Unit;

  class Wire() {
    private var sigVal = false;
    private var actions: List[Action] = List();
    def getSignal = sigVal;
    def setSignal(s: Boolean) =
      if (s != sigVal) {
        sigVal = s;
        actions.foreach(action => action());
      }
    def addAction(a: Action) = {
      actions = a :: actions; a()
    }
  }

  abstract class Simulation() {
    private type Agenda = List[Tuple2[Int, Action]];
    private var agenda: Agenda = List();
    private var curtime = 0;
    def currentTime: Int = curtime;

    def afterDelay(delay: Int)(action: Action): Unit = {
      def insert(ag: Agenda, time: Int): Agenda = ag match {
        case List() =>
          List((time, action))
        case (t, act) :: ag1 =>
          if (time < t) (time, action) :: ag
          else (t, act) :: insert(ag1, time)
      }
      agenda = insert(agenda, curtime + delay)
    }

    private def next: Unit = agenda match {
      case List() => ()
      case (time, action) :: ag1 => {
        agenda = ag1;
        curtime = time;
        action();
      }
    }

    def run: Unit = {
      afterDelay(0){() => Console.println("*** simulation started ***"); }
      while (!agenda.isEmpty) { next }
    }
  }

  abstract class BasicCircuitSimulation() extends Simulation() {

    val InverterDelay: Int;
    val AndGateDelay: Int;
    val OrGateDelay: Int;

    def inverter(input: Wire, output: Wire): Unit = {
      def invertAction() = {
        val inputSig = input.getSignal;
        afterDelay(InverterDelay) {() => output.setSignal(!inputSig) };
      }
      input addAction invertAction _
    }

    def andGate(a1: Wire, a2: Wire, output: Wire): Unit = {
      def andAction() = {
        val a1Sig = a1.getSignal;
        val a2Sig = a2.getSignal;
        afterDelay(AndGateDelay) {() => output.setSignal(a1Sig & a2Sig) };
      }
      a1 addAction andAction _
      a2 addAction andAction _
    }

    def orGate(o1: Wire, o2: Wire, output: Wire): Unit = {
      def orAction() = {
        val o1Sig = o1.getSignal;
        val o2Sig = o2.getSignal;
        afterDelay(OrGateDelay) {() => output.setSignal(o1Sig | o2Sig) };
      }
      o1 addAction orAction _
      o2 addAction orAction _
    }

    def probe(name: String, wire: Wire): Unit = {
      wire addAction {() =>
        Console.println(
          name + " " + currentTime + " new-value = " + wire.getSignal);
      }
    }
  }

  abstract class CircuitSimulation() extends BasicCircuitSimulation() {

    def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
      val d = new Wire();
      val e = new Wire();
      orGate(a, b, d);
      andGate(a, b, c);
      inverter(c, e);
      andGate(d, e, s);
    }

    def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
      val s = new Wire();
      val c1 = new Wire();
      val c2 = new Wire();
      halfAdder(a, cin, s, c1);
      halfAdder(b, s, sum, c2);
      orGate(c1, c2, cout);
    }
  }

  class Test() extends CircuitSimulation() {

    val InverterDelay = 1;
    val AndGateDelay = 3;
    val OrGateDelay = 5;

    def invert = {
      val ain  = new Wire();
      val cout = new Wire();
      inverter(ain, cout);

      def result = if (cout.getSignal) 1 else 0;

      def test(a: Int) = {
        ain setSignal (if (a == 0) false else true);
        run;
        Console.println("!" + a + " = " + result);
        Console.println;
      }

      probe("out  ", cout);

      test(0);
      test(1);
    }

    def and = {
      val ain  = new Wire();
      val bin  = new Wire();
      val cout = new Wire();
      andGate(ain, bin, cout);

      def result = if (cout.getSignal) 1 else 0;

      def test(a: Int, b: Int) = {
        ain setSignal (if (a == 0) false else true);
        bin setSignal (if (b == 0) false else true);
        run;
        Console.println(a + " & " + b + " = " + result);
        Console.println;
      }

      probe("out  ", cout);
      Console.println;

      test(0,0);
      test(0,1);
      test(1,0);
      test(1,1);
    }

    def or = {
      val ain  = new Wire();
      val bin  = new Wire();
      val cout = new Wire();
      orGate(ain, bin, cout);

      def result = if (cout.getSignal) 1 else 0;

      def test(a: Int, b: Int) = {
        ain setSignal (if (a == 0) false else true);
        bin setSignal (if (b == 0) false else true);
        run;
        Console.println(a + " | " + b + " = " + result);
        Console.println;
      }

      probe("out  ", cout);
      Console.println;

      test(0,0);
      test(0,1);
      test(1,0);
      test(1,1);
    }

    def half = {
      val ain  = new Wire();
      val bin  = new Wire();
      val sout = new Wire();
      val cout = new Wire();
      halfAdder(ain, bin, sout, cout);

      def result =
        ((if (sout.getSignal) 1 else 0) +
         (if (cout.getSignal) 2 else 0));

      def test(a: Int, b: Int) = {
        ain setSignal (if (a == 0) false else true);
        bin setSignal (if (b == 0) false else true);
        run;
        Console.println(a + " + " + b + " = " + result);
        Console.println;
      }

      probe("sum  ", sout);
      probe("carry", cout);
      Console.println;

      test(0,0);
      test(0,1);
      test(1,0);
      test(1,1);
    }

    def full = {
      val ain  = new Wire();
      val bin  = new Wire();
      val cin  = new Wire();
      val sout = new Wire();
      val cout = new Wire();
      fullAdder(ain, bin, cin, sout, cout);

      def result =
        ((if (sout.getSignal) 1 else 0) +
         (if (cout.getSignal) 2 else 0));

      def test(a: Int, b: Int, c: Int) = {
        ain setSignal (if (a == 0) false else true);
        bin setSignal (if (b == 0) false else true);
        cin setSignal (if (c == 0) false else true);
        run;
        Console.println(a + " + " + b + " + " + c + " = " + result);
        Console.println;
      }

      probe("sum  ", sout);
      probe("carry", cout);
      Console.println;

      test(0,0,0);
      test(0,0,1);
      test(0,1,0);
      test(0,1,1);
      test(1,0,0);
      test(1,0,1);
      test(1,1,0);
      test(1,1,1);
    }
  }

  def test = {
    val sim = new Test();
    sim.invert;
    sim.and;
    sim.or;
    sim.half;
    sim.full;
  }
}

//############################################################################

class Simulator() {

  type Action = () => Unit;
  type Agenda = List[Tuple2[Int, Action]];

  private var agenda: Agenda = List();
  private var curtime = 0;

  def afterDelay(delay: Int)(action: Action) = {
    def insert(ag: Agenda, time: Int): Agenda = ag match {
      case List() =>
        List((time, action))
      case (t, act) :: ag1 =>
        if (time < t) (time, action) :: ag
        else (t, act) :: insert(ag1, time)
    }
    agenda = insert(agenda, curtime + delay)
  }

  def next: Unit = agenda match {
    case List() => ()
    case (time, action) :: rest => {
      agenda = rest;
      curtime = time;
      action();
    }
  }

  protected def currentTime: Int = curtime;

  def run = {
    afterDelay(0){() => Console.println("*** simulation started ***"); }
    while (!agenda.isEmpty) { next }
  }
}

class Wire() {
  private var sigVal = false;
  private var actions: List[() => Unit] = List();
  def getSignal = sigVal;
  def setSignal(s: Boolean) =
    if (s != sigVal) {
      sigVal = s;
      actions.foreach(action => action());
    }
  def addAction(a: () => Unit) = {
    actions = a :: actions;
    a()
  }
}

abstract class BasicCircuitSimulator() extends Simulator() {

  def probe(name: String, wire: Wire): Unit = {
    wire addAction {() =>
      Console.println(
        name + " " + currentTime + " new-value = " + wire.getSignal);
    }
  }

  val InverterDelay: Int;
  val AndGateDelay: Int;
  val OrGateDelay: Int;

  def inverter(input: Wire, output: Wire) = {
    def invertAction() = {
      val inputSig = input.getSignal;
      afterDelay(InverterDelay) {() => output.setSignal(!inputSig) };
    }
    input addAction invertAction _
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) = {
    def andAction() = {
      val a1Sig = a1.getSignal;
      val a2Sig = a2.getSignal;
      afterDelay(AndGateDelay) {() => output.setSignal(a1Sig & a2Sig) };
    }
    a1 addAction andAction _
    a2 addAction andAction _
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) = {
    def orAction() = {
      val a1Sig = a1.getSignal;
      val a2Sig = a2.getSignal;
      afterDelay(OrGateDelay) {() => output.setSignal(a1Sig | a2Sig) };
    }
    a1 addAction orAction _
    a2 addAction orAction _
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) = {
    val w1 = new Wire();
    val w2 = new Wire();
    val w3 = new Wire();
    inverter(a1, w1);
    inverter(a2, w2);
    andGate(w1, w2, w3);
    inverter(w3, output);
  }
}

abstract class CircuitSimulator() extends BasicCircuitSimulator() {
  def demux2(in: Wire, ctrl: List[Wire], out: List[Wire]) : Unit = {
    val ctrlN = ctrl.map(w => { val iw = new Wire(); inverter(w,iw); iw});
    val w0 = new Wire();
	val w1 = new Wire();
	val w2 = new Wire();
	val w3 = new Wire();

    andGate(in, ctrl(1), w3);
    andGate(in, ctrl(1), w2);
    andGate(in, ctrlN(1), w1);
    andGate(in, ctrlN(1), w0);

    andGate(w3, ctrl(0), out(3));
    andGate(w2, ctrlN(0), out(2));
    andGate(w1, ctrl(0), out(1));
    andGate(w0, ctrlN(0), out(0));
  }

  def connect(in: Wire, out: Wire) = {
    in addAction {() => out.setSignal(in.getSignal); }
  }

  def demux(in: Wire, ctrl: List[Wire], out: List[Wire]): Unit = ctrl match {
    case List() => connect(in, out.head);
    case c :: rest =>
      val c_ = new Wire();
      val w1 = new Wire();
      val w2 = new Wire();
      inverter(c, c_);
      andGate(in, c_, w1);
      andGate(in, c, w2);
      demux(w1, rest, out.drop(out.length / 2));
      demux(w2, rest, out.take(out.length / 2));
  }
}

class Main() extends CircuitSimulator() {

  val InverterDelay = 1;
  val AndGateDelay = 3;
  val OrGateDelay = 5;

  def main = {
    val n = 3;
    val outNum = 1 << n;

    val in = new Wire();
    val ctrl = for (x <- range(0,n)) yield { new Wire() };
    val out = for (x <- range(0,outNum)) yield { new Wire() };

    demux(in, ctrl.reverse, out.reverse);

    probe("in", in);
    for ((x,c) <- range(0,n) zip ctrl) { probe("ctrl" + x, c) }
    for ((x,o) <- range(0,outNum) zip out) { probe("out" + x, o) }

    in.setSignal(true);
    run;
    ctrl(0).setSignal(true);
    run;
    ctrl(1).setSignal(true);
    run;
    ctrl(2).setSignal(true);
    run;
    ctrl(0).setSignal(false);
    run;
  }
}

//############################################################################

object Test {
  def main(args: Array[String]): Unit = {
    M0.test;
    M1.test;
    M2.test;
    M3.test;
    M4.test;
    M5.test;
    new Main().main;
    ()
  }
}

//############################################################################
