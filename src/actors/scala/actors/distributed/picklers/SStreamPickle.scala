package scala.actors.distributed.picklers;

import scala.collection.mutable.HashMap;

import java.io.StringReader;
import java.io.StringWriter;

/**
  Pickler combinators.

  Author: Philipp Haller <philipp.haller@epfl.ch>
  */

object SStreamPickle {
  abstract class PU[t] {
    def appP(a: t, state: OutStream): OutStream;
    def appU(state: InStream): Pair[t,InStream];
  }

  //def pickle[t](p: PU[t], a: t): OutStream =
  //  p.appP(a, "");

  def unpickle[t](p: PU[t], stream: InStream): t =
    p.appU(stream)._1;

  def lift[t](x: t): PU[t] = new PU[t] {
    def appP(a: t, state: OutStream): OutStream = state;
    def appU(state: InStream) = Pair(x, state);
  }

  def sequ[t,u](f: u => t, pa: PU[t], k: t => PU[u]): PU[u] = new PU[u] {
    def appP(b: u, s: OutStream): OutStream = {
      val a = f(b)
      val sPrime = pa.appP(a, s)
      val pb = k(a)
      val sPrimePrime = pb.appP(b, sPrime)
      sPrimePrime
    }
    def appU(s: InStream): Pair[u,InStream] = {
      val resPa = pa.appU(s)
      val a = resPa._1
      val sPrime = resPa._2
      val pb = k(a)
      pb.appU(sPrime)
    }
  }

  def pair[a,b](pa: PU[a], pb: PU[b]): PU[Pair[a,b]] = {
    def fst(p: Pair[a,b]): a = p._1;
    def snd(p: Pair[a,b]): b = p._2;
    sequ(fst, pa, (x: a) => sequ(snd, pb, (y: b) => lift(Pair(x, y))))
  }

  def triple[a,b,c](pa: PU[a], pb: PU[b], pc: PU[c]): PU[Triple[a,b,c]] = {
    def fst(p: Triple[a,b,c]): a = p._1;
    def snd(p: Triple[a,b,c]): b = p._2;
    def trd(p: Triple[a,b,c]): c = p._3;

    sequ(fst, pa,
         (x: a) => sequ(snd, pb,
         (y: b) => sequ(trd, pc,
         (z: c) => lift(Triple(x, y, z)))))
  }

  def wrap[a,b](i: a => b, j: b => a, pa: PU[a]): PU[b] =
    sequ(j, pa, (x: a) => lift(i(x)));

  def unit: PU[unit] =
    lift(unit);

  def pad(s: String, req: int): String = {
    val buf = new StringBuffer
    for (val i <- List.range(1, req-s.length+1))
      buf append "0"
    (buf append s).toString
  }
  def encode(i: int): String = pad(Integer.toHexString(i), 8);
  def decode(s: String): int = Integer.decode("0x" + s).intValue();

  def int: PU[int] = new PU[int] {
    def appP(n: int, s: OutStream): OutStream = {
      s.write(encode(n))
      s
    }
    def appU(s: InStream): Pair[int,InStream] = {
      val substr = s.read(8)
      //Console.println("unpickling " + substr)
      Pair(decode(substr), s)
    }
  }

  def char: PU[char] = new PU[char] {
    def appP(b: char, s: OutStream): OutStream = {
      s.write(b)
      s
    }
    def appU(s: InStream): Pair[char,InStream] = {
      val carr = new Array[char](1)
      s.read(carr)
      //Console.println("unpickling " + carr(0))
      Pair(carr(0), s)
    }
  }

  def bool: PU[boolean] = {
    def toEnum(b: boolean) = if (b) 1 else 0;
    def fromEnum(n: int) = if (n == 0) false else true;
    wrap(fromEnum, toEnum, nat)
  }

  def fixedList[a](pa: PU[a])(n: int): PU[List[a]] = {
    def pairToList(p: Pair[a,List[a]]): List[a] =
      p._1 :: p._2;
    def listToPair(l: List[a]): Pair[a,List[a]] =
      l match { case x :: xs => Pair(x, xs) }

    if (n == 0) lift(Nil)
    else
      wrap(pairToList, listToPair, pair(pa, fixedList(pa)(n-1)))
  }

  def list[a](pa: PU[a]): PU[List[a]] =
    sequ((l: List[a])=>l.length, nat, fixedList(pa));

  def string: PU[String] =
    wrap(List.toString, (str: String)=>str.toCharArray().toList, list(char));

  def alt[a](tag: a => int, ps: List[PU[a]]): PU[a] =
    sequ(tag, int, ps.apply);

  def data[a](tag: a => int, ps: List[()=>PU[a]]): PU[a] =
    sequ(tag, nat, (x: int)=> ps.apply(x)());

  def option[a](pa: PU[a]): PU[Option[a]] = {
    def tag(x: Option[a]) = x match {
      case None => 0
      case Some(y) => 1
    }
    def fromSome(x: Option[a]) = x match {
      case Some(y) => y
      case None => null
    }
    def toSome(x: a): Option[a] = Some(x);
    val pnone: PU[Option[a]] = lift(None)
    alt(tag, List(pnone, wrap(toSome, fromSome, pa)))
  }

  def byteString(b: int) =
    pad(Integer.toHexString(b), 2);

  def natString(x: int): String = {
    val buf = new StringBuffer

    def writeNatPrefix(x: int): unit = {
      val y = x >>> 7;
      if (y != 0) writeNatPrefix(y);
      buf.append(byteString((x & 0x7f) | 0x80));
    }

    val y = x >>> 7;
    if (y != 0) writeNatPrefix(y);
    buf.append(byteString(x & 0x7f));
    buf.toString()
  }

  def nat: PU[int] = new PU[int] {
    def appP(n: int, s: OutStream): OutStream = {
      s.write(natString(n))
      s
    }
    def appU(s: InStream): Pair[int,InStream] = {
      def readNat: int = {
        var b = 0;
        var x = 0;
        do {
          b = decode(s.read(2));
          x = (x << 7) + (b & 0x7f);
        } while ((b & 0x80) != 0);
        x
      }
      Pair(readNat, s)
    }
  }

  def main(args: Array[String]) = {
    def testBase128(x: int) = {
      Console.println(x)

      val sw = new StringWriter
      val os = new OutStream(sw)
      val res = nat.appP(x, os)
      os.flush()
      Console.println(sw.toString())

      val up = nat.appU(new InStream(new StringReader(sw.toString())))
      Console.println(up._1)
    }

    testBase128(0)
    testBase128(1)
    testBase128(64)
    testBase128(127)
    testBase128(128)
    testBase128(8192)

    def pickleTest[a](x: a, pa: PU[a]) = {
      Console.println(x)

      val sw = new StringWriter
      val os = new OutStream(sw)
      val res = pa.appP(x, os)
      os.flush()
      Console.println(sw.toString())

      val up = pa.appU(new InStream(new StringReader(sw.toString())))
      Console.println(up._1)
    }

    pickleTest(List(1, 7, 13), list(nat))

    pickleTest(List(false, true, true), list(bool))

    pickleTest("Hello", string)


    val personPU = wrap((p: Pair[String,int]) => Person(p._1, p._2), (p: Person) => Pair(p.name, p.age), pair(string, nat));
    val p = Person("Philipp", 25)
    pickleTest(p, personPU)


    val x = Var("x");
    val i = Lam("x", x);
    val k = Lam("x", Lam("y", x));
    val kki = App(k, App(k, i));

    def varPU: PU[Term] =
      wrap(Var,
           (t: Term)=> t match {case Var(x)=>x},
            string);
    def lamPU: PU[Term] =
      wrap((p: Pair[String,Term])=>Lam(p._1, p._2),
           (t: Term)=> t match {case Lam(s, t)=>Pair(s, t)},
            pair(string, termPU));
    def appPU: PU[Term] =
      wrap((p: Pair[Term,Term])=>App(p._1, p._2),
           (t: Term)=> t match {case App(t1, t2)=>Pair(t1, t2)},
            pair(termPU, termPU));
    def termPU: PU[Term] =
      data((t: Term)=> t match {case Var(_)=>0; case Lam(_,_)=>1; case App(_,_)=>2},
            List(()=>varPU, ()=>lamPU, ()=>appPU));

    pickleTest(k, termPU)
    pickleTest(kki, termPU)
  }

  case class Person(name: String, age: int);
}

abstract class Term;
case class Var(s: String) extends Term;
case class Lam(s: String, t: Term) extends Term;
case class App(t1: Term, t2: Term) extends Term;


object ShareStreamPickle {
  abstract class SPU[t] {
    def appP(a: t, state: PicklerState): PicklerState;
    def appU(state: UnPicklerState): Pair[t, UnPicklerState];
  }

  //def pickle[t](p: SPU[t], a: t): String =
  //  p.appP(a, new PicklerState("", new PicklerEnv)).stream;

  //def unpickle[t](p: SPU[t], stream: String): t =
  //  p.appU(new UnPicklerState(stream, new UnPicklerEnv))._1;

  class PicklerEnv extends HashMap[Any, int] {
    private var cnt: int = 64;
    def nextLoc() = { cnt = cnt + 1; cnt };
  }

  class UnPicklerEnv extends HashMap[int, Any] {
    private var cnt: int = 64;
    def nextLoc() = { cnt = cnt + 1; cnt };
  }

  class PicklerState(val stream: OutStream, val dict: PicklerEnv) {}
  class UnPicklerState(val stream: InStream, val dict: UnPicklerEnv) {}

  abstract class RefDef;
  case class Ref() extends RefDef;
  case class Def() extends RefDef;

  def refDef: SStreamPickle.PU[RefDef] = new SStreamPickle.PU[RefDef] {
    def appP(b: RefDef, s: OutStream): OutStream =
      b match {
        case Ref() => s.write("0"); s
        case Def() => s.write("1"); s
      };
    def appU(s: InStream): Pair[RefDef, InStream] =
      if (s.readChar == '0') Pair(Ref(), s)
      else Pair(Def(), s);
  }

  def share[a](pa: SPU[a]): SPU[a] = new SPU[a] {
    def appP(v: a, state: PicklerState): PicklerState = {
      /*
      - is there some value equal to v associated with a location l in the pickle environment?
      - yes: write REF-tag to outstream together with l
      - no:
          write DEF-tag to outstream
          record current location l of outstream
          --> serialize value
          add entry to pickle environment, mapping v onto l
      */
      val pe = state.dict
      pe.get(v) match {
        case None =>
          //Console.println("" + v + " is new")
	  //Console.println("writing DEF...")
          val sPrime = refDef.appP(Def(), state.stream)
          val l = pe.nextLoc()

          //Console.println("applying pickler to state " + sPrime)
          val sPrimePrime = pa.appP(v, new PicklerState(sPrime, pe))

          //Console.println("updating dict (" + l + ") for " + v)
          pe.update(v, l)

          return sPrimePrime
        case Some(l) =>
          //Console.println("writing REF...")
          val sPrime = refDef.appP(Ref(), state.stream)
          //Console.println("writing location to state " + sPrime)
          return new PicklerState(SStreamPickle.nat.appP(l, sPrime), pe)
      }
    }
    def appU(state: UnPicklerState): Pair[a, UnPicklerState] = {
      /*
      - first, read tag (i.e. DEF or REF)
      - if REF:
          read location l
          look up resulting value in unpickler environment
      - if DEF:
          record location l of input stream
          --> deserialize value v with argument deserializer
          add entry to unpickler environment, mapping l onto v
      */
      val upe = state.dict
      val res = refDef.appU(state.stream)
      res._1 match {
        case Def() =>
          val l = upe.nextLoc
          val res2 = pa.appU(new UnPicklerState(res._2, upe))
          upe.update(l, res2._1)
          return res2
        case Ref() =>
          val res2 = SStreamPickle.nat.appU(res._2)  // read location
          upe.get(res2._1) match {     // lookup value in unpickler env
            case None => error("invalid unpickler environment"); return null
            case Some(v) => return Pair(v.asInstanceOf[a], new UnPicklerState(res2._2, upe))
          }
      }
    }
  }

  def lift[t](x: t): SPU[t] = new SPU[t] {
    def appP(a: t, state: PicklerState): PicklerState = state;
    def appU(state: UnPicklerState) = Pair(x, state);
  }

  def sequ[t,u](f: u => t, pa: SPU[t], k: t => SPU[u]): SPU[u] = new SPU[u] {
    def appP(b: u, s: PicklerState): PicklerState = {
      val a = f(b)
      //Console.println("pickling " + a + ", s: " + s.stream)
      val sPrime = pa.appP(a, s)
      val pb = k(a)
      //Console.println("pickling " + b + ", s: " + s.stream)
      pb.appP(b, sPrime)
    }
    def appU(s: UnPicklerState): Pair[u, UnPicklerState] = {
      val resPa = pa.appU(s)
      val a = resPa._1
      val sPrime = resPa._2
      val pb = k(a)
      pb.appU(sPrime)
    }
  }

  def pair[a,b](pa: SPU[a], pb: SPU[b]): SPU[Pair[a,b]] = {
    def fst(p: Pair[a,b]): a = p._1;
    def snd(p: Pair[a,b]): b = p._2;
    sequ(fst, pa, (x: a) => sequ(snd, pb, (y: b) => lift(Pair(x, y))))
  }

  def wrap[a,b](i: a => b, j: b => a, pa: SPU[a]): SPU[b] =
    sequ(j, pa, (x: a) => lift(i(x)));

  def char: SPU[char] = new SPU[char] {
    def appP(b: char, s: PicklerState): PicklerState = {
      s.stream.write(b)
      new PicklerState(s.stream, s.dict)
    }
    def appU(s: UnPicklerState): Pair[char, UnPicklerState] =
      Pair(s.stream.readChar, new UnPicklerState(s.stream, s.dict));
  }

  def pad(s: String, req: int): String = {
    val buf = new StringBuffer
    for (val i <- List.range(1, req-s.length+1))
      buf append "0"
    (buf append s).toString
  }
  def encode(i: int): String = pad(Integer.toHexString(i), 8);
  def decode(s: String): int = Integer.decode("0x" + s).intValue();

  def byteString(b: int) =
    pad(Integer.toHexString(b), 2);

  def natString(x: int): String = {
    val buf = new StringBuffer

    def writeNatPrefix(x: int): unit = {
      val y = x >>> 7;
      if (y != 0) writeNatPrefix(y);
      buf.append(byteString((x & 0x7f) | 0x80));
    }

    val y = x >>> 7;
    if (y != 0) writeNatPrefix(y);
    buf.append(byteString(x & 0x7f));
    buf.toString()
  }

  def nat: SPU[int] = new SPU[int] {
    def appP(n: int, s: PicklerState): PicklerState = {
      s.stream.write(natString(n))
      new PicklerState(s.stream, s.dict)
    }
    def appU(s: UnPicklerState): Pair[int,UnPicklerState] = {
      def readNat: int = {
        var b = 0;
        var x = 0;
        do {
          b = decode(s.stream.read(2));
          x = (x << 7) + (b & 0x7f);
        } while ((b & 0x80) != 0);
        x
      }
      Pair(readNat, new UnPicklerState(s.stream, s.dict))
    }
  }

  def fixedList[a](pa: SPU[a])(n: int): SPU[List[a]] = {
    def pairToList(p: Pair[a,List[a]]): List[a] =
      p._1 :: p._2;
    def listToPair(l: List[a]): Pair[a,List[a]] =
      l match { case x :: xs => Pair(x, xs) }

    if (n == 0) lift(Nil)
    else
      wrap(pairToList, listToPair, pair(pa, fixedList(pa)(n-1)))
  }

  def list[a](pa: SPU[a]): SPU[List[a]] =
    sequ((l: List[a])=>l.length, nat, fixedList(pa));

  def string: SPU[String] =
    wrap(List.toString, (str: String)=>str.toCharArray().toList, list(char));

  def alt[a](tag: a => int, ps: List[SPU[a]]): SPU[a] =
    sequ(tag, nat, ps.apply);

  def data[a](tag: a => int, ps: List[()=>SPU[a]]): SPU[a] =
    sequ(tag, nat, (x: int)=> ps.apply(x)());

  def main(args: Array[String]) = {
    def pickleTest[a](x: a, pa: SPU[a]) = {
      Console.println(x)

      val sw = new StringWriter
      val os = new OutStream(sw)
      val res = pa.appP(x, new PicklerState(os, new PicklerEnv))
      os.flush()
      Console.println(sw.toString())

      val up = pa.appU(new UnPicklerState(new InStream(new StringReader(sw.toString())), new UnPicklerEnv))
      Console.println(up._1)
    }

    val x = Var("x");
    val i = Lam("x", x);
    val k = Lam("x", Lam("y", x));
    val kki = App(k, App(k, i));

    def varSPU: SPU[Term] = wrap(Var,
                                 (t: Term)=> t match {case Var(x)=>x},
                                 string);

    def lamSPU: SPU[Term] = wrap((p: Pair[String,Term])=>Lam(p._1, p._2),
                                 (t: Term)=> t match {case Lam(s, t)=>Pair(s, t)},
                                 pair(string, termSPU));

    def appSPU: SPU[Term] = wrap((p: Pair[Term,Term])=>App(p._1, p._2),
                                 (t: Term)=> t match {case App(t1, t2)=>Pair(t1, t2)},
                                 pair(termSPU, termSPU));

    def termSPU: SPU[Term] = data((t: Term)=> t match {case Var(_)=>0; case Lam(_,_)=>1; case App(_,_)=>2},
                                        List(()=>varSPU, ()=>lamSPU, ()=>appSPU));

    def termSPUshared: SPU[Term] = share(data((t: Term)=> t match {case Var(_)=>0; case Lam(_,_)=>1; case App(_,_)=>2},
                                        List(()=>varSPU, ()=>lamSPU, ()=>appSPU)));

    pickleTest(k, termSPU)
    pickleTest(k, termSPUshared)
    pickleTest(kki, termSPU)
    pickleTest(kki, termSPUshared)
  }
}
