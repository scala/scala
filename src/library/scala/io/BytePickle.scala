/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.io

import scala.collection.mutable

/**
 * Pickler combinators.
 * Based on a Haskell library by Andrew Kennedy,
 * see <a href="http://research.microsoft.com/~akenn/fun/"
 * target="_top">http://research.microsoft.com/~akenn/fun/</a>.
 *
 * @author  Philipp Haller
 * @version 1.1
 */
@deprecated("This class will be removed.", "2.10.0")
object BytePickle {
  abstract class SPU[T] {
    def appP(a: T, state: PicklerState): PicklerState
    def appU(state: UnPicklerState): (T, UnPicklerState)
  }

  def pickle[T](p: SPU[T], a: T): Array[Byte] =
    p.appP(a, new PicklerState(new Array[Byte](0), new PicklerEnv)).stream

  def unpickle[T](p: SPU[T], stream: Array[Byte]): T =
    p.appU(new UnPicklerState(stream, new UnPicklerEnv))._1

  abstract class PU[T] {
    def appP(a: T, state: Array[Byte]): Array[Byte]
    def appU(state: Array[Byte]): (T, Array[Byte])
  }

  def upickle[T](p: PU[T], a: T): Array[Byte] =
    p.appP(a, new Array[Byte](0))

  def uunpickle[T](p: PU[T], stream: Array[Byte]): T =
    p.appU(stream)._1

  class PicklerEnv extends mutable.HashMap[Any, Int] {
    private var cnt: Int = 64
    def nextLoc() = { cnt += 1; cnt }
  }

  class UnPicklerEnv extends mutable.HashMap[Int, Any] {
    private var cnt: Int = 64
    def nextLoc() = { cnt += 1; cnt }
  }

  class PicklerState(val stream: Array[Byte], val dict: PicklerEnv)
  class UnPicklerState(val stream: Array[Byte], val dict: UnPicklerEnv)

  abstract class RefDef
  case class Ref() extends RefDef
  case class Def() extends RefDef

  def refDef: PU[RefDef] = new PU[RefDef] {
    def appP(b: RefDef, s: Array[Byte]): Array[Byte] =
      b match {
        case Ref() => Array.concat(s, Array[Byte](0))
        case Def() => Array.concat(s, Array[Byte](1))
      };
    def appU(s: Array[Byte]): (RefDef, Array[Byte]) =
      if (s(0) == (0: Byte)) (Ref(), s.slice(1, s.length))
      else (Def(), s.slice(1, s.length));
  }

  val REF = 0
  val DEF = 1

  def unat: PU[Int] = new PU[Int] {
    def appP(n: Int, s: Array[Byte]): Array[Byte] =
      Array.concat(s, nat2Bytes(n));
    def appU(s: Array[Byte]): (Int, Array[Byte]) = {
      var num = 0
      def readNat: Int = {
        var b = 0;
        var x = 0;
        do {
          b = s(num)
          num += 1
          x = (x << 7) + (b & 0x7f);
        } while ((b & 0x80) != 0);
        x
      }
      (readNat, s.slice(num, s.length))
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
          val sPrime = refDef.appP(Def(), state.stream)
          val l = pe.nextLoc()

          val sPrimePrime = pa.appP(v, new PicklerState(sPrime, pe))

          pe.update(v, l)

          return sPrimePrime
        case Some(l) =>
          val sPrime = refDef.appP(Ref(), state.stream)

          return new PicklerState(unat.appP(l, sPrime), pe)
      }
    }
    def appU(state: UnPicklerState): (a, UnPicklerState) = {
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
            case None => throw new IllegalArgumentException("invalid unpickler environment")
            case Some(v) => return (v.asInstanceOf[a], new UnPicklerState(res2._2, upe))
          }
      }
    }
  }

  def ulift[t](x: t): PU[t] = new PU[t] {
    def appP(a: t, state: Array[Byte]): Array[Byte] =
      if (x != a) throw new IllegalArgumentException("value to be pickled (" + a + ") != " + x)
      else state;
    def appU(state: Array[Byte]) = (x, state)
  }

  def lift[t](x: t): SPU[t] = new SPU[t] {
    def appP(a: t, state: PicklerState): PicklerState =
      if (x != a) { /*throw new IllegalArgumentException("value to be pickled (" + a + ") != " + x);*/ state }
      else state;
    def appU(state: UnPicklerState) = (x, state)
  }

  def usequ[t,u](f: u => t, pa: PU[t], k: t => PU[u]): PU[u] = new PU[u] {
    def appP(b: u, s: Array[Byte]): Array[Byte] = {
      val a = f(b)
      val sPrime = pa.appP(a, s)
      val pb = k(a)
      val sPrimePrime = pb.appP(b, sPrime)
      sPrimePrime
    }
    def appU(s: Array[Byte]): (u, Array[Byte]) = {
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
      val sPrime = pa.appP(a, s)
      val pb = k(a)
      pb.appP(b, sPrime)
    }
    def appU(s: UnPicklerState): (u, UnPicklerState) = {
      val resPa = pa.appU(s)
      val a = resPa._1
      val sPrime = resPa._2
      val pb = k(a)
      pb.appU(sPrime)
    }
  }

  def upair[a,b](pa: PU[a], pb: PU[b]): PU[(a,b)] = {
    def fst(p: (a,b)): a = p._1
    def snd(p: (a,b)): b = p._2
    usequ(fst, pa, (x: a) => usequ(snd, pb, (y: b) => ulift((x, y))))
  }

  def pair[a,b](pa: SPU[a], pb: SPU[b]): SPU[(a,b)] = {
    def fst(p: (a,b)): a = p._1
    def snd(p: (a,b)): b = p._2
    sequ(fst, pa, (x: a) => sequ(snd, pb, (y: b) => lift((x, y))))
  }

  def triple[a,b,c](pa: SPU[a], pb: SPU[b], pc: SPU[c]): SPU[(a,b,c)] = {
    def fst(p: (a,b,c)): a = p._1
    def snd(p: (a,b,c)): b = p._2
    def trd(p: (a,b,c)): c = p._3

    sequ(fst, pa,
         (x: a) => sequ(snd, pb,
         (y: b) => sequ(trd, pc,
         (z: c) => lift((x, y, z)))))
  }

  def uwrap[a,b](i: a => b, j: b => a, pa: PU[a]): PU[b] =
    usequ(j, pa, (x: a) => ulift(i(x)))

  def wrap[a,b](i: a => b, j: b => a, pa: SPU[a]): SPU[b] =
    sequ(j, pa, (x: a) => lift(i(x)))

  def appendByte(a: Array[Byte], b: Int): Array[Byte] =
    Array.concat(a, Array(b.toByte))

  def nat2Bytes(x: Int): Array[Byte] = {
    val buf = new mutable.ArrayBuffer[Byte]
    def writeNatPrefix(x: Int) {
      val y = x >>> 7;
      if (y != 0) writeNatPrefix(y);
      buf += ((x & 0x7f) | 0x80).asInstanceOf[Byte];
    }
    val y = x >>> 7;
    if (y != 0) writeNatPrefix(y);
    buf += (x & 0x7f).asInstanceOf[Byte];
    buf.toArray
  }

  def nat: SPU[Int] = new SPU[Int] {
    def appP(n: Int, s: PicklerState): PicklerState = {
      new PicklerState(Array.concat(s.stream, nat2Bytes(n)), s.dict);
    }
    def appU(s: UnPicklerState): (Int,UnPicklerState) = {
      var num = 0
      def readNat: Int = {
        var b = 0
        var x = 0
        do {
          b = s.stream(num)
          num += 1
          x = (x << 7) + (b & 0x7f);
        } while ((b & 0x80) != 0);
        x
      }
      (readNat, new UnPicklerState(s.stream.slice(num, s.stream.length), s.dict))
    }
  }

  def byte: SPU[Byte] = new SPU[Byte] {
    def appP(b: Byte, s: PicklerState): PicklerState =
      new PicklerState(Array.concat(s.stream, Array(b)), s.dict)
    def appU(s: UnPicklerState): (Byte, UnPicklerState) =
      (s.stream(0), new UnPicklerState(s.stream.slice(1, s.stream.length), s.dict));
  }

  def string: SPU[String] = share(wrap(
    (a: Array[Byte]) => (Codec fromUTF8 a).mkString,
    (s: String) => Codec toUTF8 s,
    bytearray
  ))

  def bytearray: SPU[Array[Byte]] = {
    wrap((l:List[Byte]) => l.toArray, (_.toList), list(byte))
  }

  def bool: SPU[Boolean] = {
    def toEnum(b: Boolean) = if (b) 1 else 0
    def fromEnum(n: Int) = if (n == 0) false else true
    wrap(fromEnum, toEnum, nat)
  }

  def ufixedList[A](pa: PU[A])(n: Int): PU[List[A]] = {
    def pairToList(p: (A, List[A])): List[A] =
      p._1 :: p._2;
    def listToPair(l: List[A]): (A, List[A]) =
      (l: @unchecked) match { case x :: xs => (x, xs) }

    if (n == 0) ulift(Nil)
    else
      uwrap(pairToList, listToPair, upair(pa, ufixedList(pa)(n-1)))
  }

  def fixedList[a](pa: SPU[a])(n: Int): SPU[List[a]] = {
    def pairToList(p: (a,List[a])): List[a] =
      p._1 :: p._2;
    def listToPair(l: List[a]): (a,List[a]) =
      (l: @unchecked) match { case x :: xs => (x, xs) }

    if (n == 0) lift(Nil)
    else
      wrap(pairToList, listToPair, pair(pa, fixedList(pa)(n-1)))
  }

  def list[a](pa: SPU[a]): SPU[List[a]] =
    sequ((l: List[a])=>l.length, nat, fixedList(pa));

  def ulist[a](pa: PU[a]): PU[List[a]] =
    usequ((l:List[a]) => l.length, unat, ufixedList(pa));

  def data[a](tag: a => Int, ps: List[()=>SPU[a]]): SPU[a] =
    sequ(tag, nat, (x: Int)=> ps.apply(x)());
}
