package actors.examples.counter

import actors.multi.Pid

abstract class CounterMessage
case class Incr() extends CounterMessage
case class Value(p: Pid) extends CounterMessage
case class Result(v: int) extends CounterMessage
