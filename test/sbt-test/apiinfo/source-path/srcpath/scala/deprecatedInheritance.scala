package scala

private[scala] // for now, this needs to be generalized to communicate other modifier deltas
class deprecatedInheritance(message: String = "", since: String = "") extends scala.annotation.StaticAnnotation
