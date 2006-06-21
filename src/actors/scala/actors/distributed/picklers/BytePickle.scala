package scala.actors.distributed.picklers

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

/**
  Pickler combinators.

  Author: Philipp Haller <philipp.haller@epfl.ch>
  */

object BytePickle {
  class PicklerState(val stream: Array[byte], val dict: PicklerEnv) {}
  class UnPicklerState(val stream: Array[byte], val dict: UnPicklerEnv) {}

  abstract class PU[t] {
    def appP(a: t, state: Array[byte]): Array[byte];
    def appU(state: Array[byte]): Pair[t, Array[byte]];
  }

  abstract class SPU[t] {
    def appP(a: t, state: PicklerState): PicklerState;
    def appU(state: UnPicklerState): Pair[t, UnPicklerState];
  }

  class PicklerEnv extends HashMap[Any, int] {
    private var cnt: int = 64;
    def nextLoc() = { cnt = cnt + 1; cnt };
  }

  class UnPicklerEnv extends HashMap[int, Any] {
    private var cnt: int = 64;
    def nextLoc() = { cnt = cnt + 1; cnt };
  }

  abstract class RefDef;
  case class Ref() extends RefDef;
  case class Def() extends RefDef;

  def refDef: PU[RefDef] = new PU[RefDef] {
    def appP(b: RefDef, s: Array[byte]): Array[byte] =
      b match {
        case Ref() => Array.concat(s, (List[byte](0)).toArray)
        case Def() => Array.concat(s, (List[byte](1)).toArray)
      };
    def appU(s: Array[byte]): Pair[RefDef, Array[byte]] =
      if (s(0) == 0) Pair(Ref(), s.subArray(1, s.length))
      else Pair(Def(), s.subArray(1, s.length));
  }

  val REF = 0
  val DEF = 1

  def unat: PU[int] = new PU[int] {
    def appP(n: int, s: Array[byte]): Array[byte] =
      Array.concat(s, nat2Bytes(n));
    def appU(s: Array[byte]): Pair[int, Array[byte]] = {
      var num = 0
      def readNat: int = {
        var b = 0;
        var x = 0;
        do {
          b = s(num)
          num = num + 1
          x = (x << 7) + (b & 0x7f);
        } while ((b & 0x80) != 0);
        x
      }
      Pair(readNat, s.subArray(num, s.length))
    }
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
          return new PicklerState(unat.appP(l, sPrime), pe)
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
          val res2 = unat.appU(res._2)  // read location
          upe.get(res2._1) match {     // lookup value in unpickler env
            case None => error("invalid unpickler environment"); return null
            case Some(v) => return Pair(v.asInstanceOf[a], new UnPicklerState(res2._2, upe))
          }
      }
    }
  }

  def upickle[t](p: PU[t], a: t): Array[byte] =
    p.appP(a, new Array[byte](0));

  def uunpickle[t](p: PU[t], stream: Array[byte]): t =
    p.appU(stream)._1;

  def pickle[t](p: SPU[t], a: t): Array[byte] =
    p.appP(a, new PicklerState(new Array[byte](0), new PicklerEnv)).stream;

  def unpickle[t](p: SPU[t], stream: Array[byte]): t =
    p.appU(new UnPicklerState(stream, new UnPicklerEnv))._1;

  def ulift[t](x: t): PU[t] = new PU[t] {
    def appP(a: t, state: Array[byte]): Array[byte] =
      if (x != a) { error("value to be pickled (" + a + ") != " + x); state }
      else state;
    def appU(state: Array[byte]) = Pair(x, state);
  }

  def lift[t](x: t): SPU[t] = new SPU[t] {
    def appP(a: t, state: PicklerState): PicklerState =
      if (x != a) { /*error("value to be pickled (" + a + ") != " + x);*/ state }
      else state;
    def appU(state: UnPicklerState) = Pair(x, state);
  }

  def usequ[t,u](f: u => t, pa: PU[t], k: t => PU[u]): PU[u] = new PU[u] {
    def appP(b: u, s: Array[byte]): Array[byte] = {
      val a = f(b)
      val sPrime = pa.appP(a, s)
      val pb = k(a)
      val sPrimePrime = pb.appP(b, sPrime)
      sPrimePrime
    }
    def appU(s: Array[byte]): Pair[u, Array[byte]] = {
      val resPa = pa.appU(s)
      val a = resPa._1
      val sPrime = resPa._2
      val pb = k(a)
      pb.appU(sPrime)
    }
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

  def upair[a,b](pa: PU[a], pb: PU[b]): PU[Pair[a,b]] = {
    def fst(p: Pair[a,b]): a = p._1;
    def snd(p: Pair[a,b]): b = p._2;
    usequ(fst, pa, (x: a) => usequ(snd, pb, (y: b) => ulift(Pair(x, y))))
  }

  def pair[a,b](pa: SPU[a], pb: SPU[b]): SPU[Pair[a,b]] = {
    def fst(p: Pair[a,b]): a = p._1;
    def snd(p: Pair[a,b]): b = p._2;
    sequ(fst, pa, (x: a) => sequ(snd, pb, (y: b) => lift(Pair(x, y))))
  }

  def triple[a,b,c](pa: SPU[a], pb: SPU[b], pc: SPU[c]): SPU[Triple[a,b,c]] = {
    def fst(p: Triple[a,b,c]): a = p._1;
    def snd(p: Triple[a,b,c]): b = p._2;
    def trd(p: Triple[a,b,c]): c = p._3;

    sequ(fst, pa,
         (x: a) => sequ(snd, pb,
         (y: b) => sequ(trd, pc,
         (z: c) => lift(Triple(x, y, z)))))
  }

  def uwrap[a,b](i: a => b, j: b => a, pa: PU[a]): PU[b] =
    usequ(j, pa, (x: a) => ulift(i(x)));

  def wrap[a,b](i: a => b, j: b => a, pa: SPU[a]): SPU[b] =
    sequ(j, pa, (x: a) => lift(i(x)));

  def appendByte(a: Array[byte], b: int): Array[byte] = {
    Array.concat(a, (List[byte](b.asInstanceOf[byte])).toArray)
  }

  def nat2Bytes(x: int): Array[byte] = {
    val buf = new ArrayBuffer[byte]
    def writeNatPrefix(x: int): unit = {
      val y = x >>> 7;
      if (y != 0) writeNatPrefix(y);
      buf += ((x & 0x7f) | 0x80).asInstanceOf[byte];
    }
    val y = x >>> 7;
    if (y != 0) writeNatPrefix(y);
    buf += (x & 0x7f).asInstanceOf[byte];
    buf.toArray
  }

  def nat: SPU[int] = new SPU[int] {
    def appP(n: int, s: PicklerState): PicklerState = {
      new PicklerState(Array.concat(s.stream, nat2Bytes(n)), s.dict);
    }
    def appU(s: UnPicklerState): Pair[int,UnPicklerState] = {
      var num = 0
      def readNat: int = {
        var b = 0;
        var x = 0;
        do {
          b = s.stream(num)
          num = num + 1
          x = (x << 7) + (b & 0x7f);
        } while ((b & 0x80) != 0);
        x
      }
      Pair(readNat, new UnPicklerState(s.stream.subArray(num, s.stream.length), s.dict))
    }
  }

  def byte: SPU[byte] = new SPU[byte] {
    def appP(b: byte, s: PicklerState): PicklerState =
      new PicklerState(Array.concat(s.stream, (List[byte](b)).toArray), s.dict);
    def appU(s: UnPicklerState): Pair[byte, UnPicklerState] =
      Pair(s.stream(0), new UnPicklerState(s.stream.subArray(1, s.stream.length), s.dict));
  }

  def string: SPU[String] =
    share(wrap((a:Array[byte]) => UTF8Codec.decode(a, 0, a.length), (s:String) => UTF8Codec.encode(s), bytearray));

  def bytearray: SPU[Array[byte]] = {
    wrap((l:List[byte]) => l.toArray, .toList, list(byte))
  }

  def bool: SPU[boolean] = {
    def toEnum(b: boolean) = if (b) 1 else 0;
    def fromEnum(n: int) = if (n == 0) false else true;
    wrap(fromEnum, toEnum, nat)
  }

  def ufixedList[a](pa: PU[a])(n: int): PU[List[a]] = {
    def pairToList(p: Pair[a,List[a]]): List[a] =
      p._1 :: p._2;
    def listToPair(l: List[a]): Pair[a,List[a]] =
      l match { case x :: xs => Pair(x, xs) }

    if (n == 0) ulift(Nil)
    else
      uwrap(pairToList, listToPair, upair(pa, ufixedList(pa)(n-1)))
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

  def ulist[a](pa: PU[a]): PU[List[a]] =
    usequ((l:List[a]) => l.length, unat, ufixedList(pa));

  def data[a](tag: a => int, ps: List[()=>SPU[a]]): SPU[a] =
    sequ(tag, nat, (x: int)=> ps.apply(x)());

  def printByteArray(a: Array[byte]) = {
    val iter = a.elements
    while (iter.hasNext) {
      val el = iter.next
      Console.print("" + el + ", ")
    }
  }

  def main(args: Array[String]) = {
    // test nat2Bytes

    Console.println(printByteArray(nat2Bytes(1)))
    Console.println(printByteArray(nat2Bytes(10)))
    Console.println(printByteArray(nat2Bytes(16)))
    Console.println(printByteArray(nat2Bytes(256)))

    Console.println(100000)
    var res = pickle(nat, 100000)
    Console.println(printByteArray(res))
    var up = unpickle(nat, res)
    Console.println(up)

    // -- int list
    val intList = List(1, 7, 13)
    Console.println(intList)
    val res9 = pickle(list(nat), intList)
    Console.println(printByteArray(res9))
    val up9 = unpickle(list(nat), res9)
    Console.println(up9)

    // ---------------
    // -- boolean list
    val bList = List(false, true, true)
    Console.println(bList)
    val res2 = pickle(list(bool), bList)
    Console.println(printByteArray(res2))
    val up2 = unpickle(list(bool), res2)
    Console.println(up2)

    // -- string
    val s = "Hello"
    Console.println(s)
    val res3 = pickle(string, s)
    Console.println(printByteArray(res3))
    val up3 = unpickle(string, res3)
    Console.println(up3)

    val personPU = wrap((p:Pair[String,int]) => Person(p._1, p._2), (p:Person) => Pair(p.name, p.age), pair(string, nat));
    val p = Person("Philipp", 25)
    Console.println(p)
    val res4 = pickle(personPU, p)
    Console.println(printByteArray(res4))
    val up4 = unpickle(personPU, res4)
    Console.println(up4)

    val x = Var("x");
    val i = Lam("x", x);
    val k = Lam("x", Lam("y", x));
    val kki = App(k, App(k, i));

    /*def varPU: PU[Term] = wrap(Var,
                               (t: Term)=> t match {case Var(x)=>x},
                               string);
    def lamPU: PU[Term] = wrap(p: Pair[String,Term]=>Lam(p._1, p._2),
                               (t: Term)=> t match {case Lam(s, t)=>Pair(s, t)},
                               pair(string, termPU));
    def appPU: PU[Term] = wrap(p: Pair[Term,Term]=>App(p._1, p._2),
                               (t: Term)=> t match {case App(t1, t2)=>Pair(t1, t2)},
                               pair(termPU, termPU));
    def termPU: PU[Term] = data((t: Term)=> t match {case Var(_)=>0; case Lam(_,_)=>1; case App(_,_)=>2},
                                    List(()=>varPU, ()=>lamPU, ()=>appPU));

    Console.println("\n" + k);
    val res5 = pickle(termPU, k);
    Console.println(res5);
    val up5 = unpickle(termPU, res5);
    Console.println(up5);

    Console.println("\n" + kki);
    val res6 = pickle(termPU, kki);
    Console.println(res6);
    Console.println("len: " + res6.length)
    val up6 = unpickle(termPU, res6);
    Console.println(up6);*/

    def varSPU: SPU[Term] = wrap(Var,
        (t: Term)=> t match {case Var(x)=>x},
        string);

    def lamSPU: SPU[Term] = wrap((p: Pair[String,Term])=>Lam(p._1, p._2),
        (t: Term)=> t match {case Lam(s, t)=>Pair(s, t)},
        pair(string, termSPU));

    def appSPU: SPU[Term] = wrap((p: Pair[Term,Term])=>App(p._1, p._2),
        (t: Term)=> t match {case App(t1, t2)=>Pair(t1, t2)},
        pair(termSPU, termSPU));

    def termSPU: SPU[Term] = share(data((t: Term)=> t match {case Var(_)=>0; case Lam(_,_)=>1; case App(_,_)=>2},
             List(()=>varSPU, ()=>lamSPU, ()=>appSPU)));

    Console.println("\n" + k);
    val res8 = pickle(termSPU, k);
    Console.println(printByteArray(res8));
    Console.println("len: " + res8.length)
    val up8 = unpickle(termSPU, res8);
    Console.println(up8);

    Console.println("\n" + kki);
    val res7 = pickle(termSPU, kki);
    Console.println(printByteArray(res7));
    Console.println("len: " + res7.length)

    val up7 = unpickle(termSPU, res7);
    Console.println(up7);
  }

  case class Person(name: String, age: int);

  abstract class Term;
  case class Var(s: String) extends Term;
  case class Lam(s: String, t: Term) extends Term;
  case class App(t1: Term, t2: Term) extends Term;
}
