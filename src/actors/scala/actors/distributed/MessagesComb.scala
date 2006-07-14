/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.distributed

import scala.io.BytePickle._

/**
 * @author Philipp Haller
 */
object MessagesComb {
  def sendPU(ser: Serializer): SPU[Send] =
    wrap((p: Pair[RemotePid,Array[byte]]) => Send(p._1, p._2),
         (s: Send) => Pair(s.rec, s.data),
         pair(ser.pid, bytearray));

  def namedSendPU: SPU[NamedSend] =
    wrap((p: Pair[Symbol,Array[byte]]) => NamedSend(p._1, p._2),
         (ns: NamedSend) => Pair(ns.sym, ns.data),
         pair(symbolPU, bytearray));

  def spawnPU(ser: Serializer): SPU[Spawn] =
    wrap((p: Pair[RemotePid,String]) => Spawn(p._1, p._2),
         (s: Spawn) => Pair(s.replyto, s.p),
         pair(ser.pid, string));

  def spawnObjectPU(ser: Serializer): SPU[SpawnObject] =
    wrap((p: Pair[RemotePid,Array[byte]]) => SpawnObject(p._1, p._2),
         (s: SpawnObject) => Pair(s.replyto, s.data),
         pair(ser.pid, bytearray));

  def exitPU(ser: Serializer): SPU[Exit1] =
    wrap((p: Triple[RemotePid,RemotePid,Symbol]) => Exit1(p._1, p._2, p._3),
         (e: Exit1) => Triple(e.from, e.to, e.reason),
         triple(ser.pid, ser.pid, symbolPU));

  def symbolPU: SPU[Symbol] =
    wrap((s: String) => Symbol(s),
         (sym: Symbol) => sym.name,
         string);
}
