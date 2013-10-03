package test

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "Cannot construct a collection of type ${Too} with elements of type ${Elem} based on a collection of type ${From}.")
trait Meh[-From, +To]

@implicitNotFound(msg = "Cannot construct a collection of type ${To} ${Elem}.")
trait Meh2[-From, +To]
