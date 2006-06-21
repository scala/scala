package scala.actors.distributed;

import scala.actors.distributed.picklers.BytePickle._;
import scala.actors.multi.Pid;

object MessagesComb {
  def sendPU(ser: Serializer): SPU[Send] =
    wrap((p: Pair[Pid,Array[byte]]) => Send(p._1, p._2),
         (s: Send) => Pair(s.rec, s.data),
         pair(ser.pid, bytearray));

  def spawnPU(ser: Serializer): SPU[Spawn] =
    wrap((p: Pair[Pid,String]) => Spawn(p._1, p._2),
         (s: Spawn) => Pair(s.replyto, s.p),
         pair(ser.pid, string));

  def symbolPU: SPU[Symbol] =
    wrap((s: String) => Symbol(s),
         (sym: Symbol) => sym.name,
         string);

  def exitPU(ser: Serializer): SPU[Exit1] =
    wrap((p: Triple[Pid,Pid,Symbol]) => Exit1(p._1, p._2, p._3),
         (e: Exit1) => Triple(e.from, e.to, e.reason),
         triple(ser.pid, ser.pid, symbolPU));

  def spawnObjectPU(ser: Serializer): SPU[SpawnObject] =
    wrap((p: Pair[Pid,Array[byte]]) => SpawnObject(p._1, p._2),
         (s: SpawnObject) => Pair(s.replyto, s.data),
         pair(ser.pid, bytearray));

  def namedSendPU: SPU[NamedSend] =
    wrap((p: Pair[Symbol,Array[byte]]) => NamedSend(p._1, p._2),
         (ns: NamedSend) => Pair(ns.sym, ns.data),
         pair(symbolPU, bytearray));
}
