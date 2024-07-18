//> using options -Werror -Wunused:nowarn -Xsource:3
//
class A { def x: Int = 3 }
class B extends A { override def x(): Int = 4 }


trait T1 { def x: String   = "1" }
trait T2 { def x(): String = "2" }

// without overrides you just get: error: class X inherits conflicting members:
//   def x: String (defined in trait T1) and
//   def x(): String (defined in trait T2)
//   (note: this can be resolved by declaring an `override` in class X.)

// Mix12a in nullary-override-3b
class Mix12b extends T1 with T2 { override def x() = "12b" }

class Mix21a extends T2 with T1 { override def x   = "21a" }
class Mix21b extends T2 with T1 { override def x() = "21b" }

import java.util.concurrent.atomic.{ AtomicMarkableReference => AMR }
trait Ref1 { def getReference: String   = "1" }
trait Ref2 { def getReference(): String = "2" }

// without overrides you just get: error: class X inherits conflicting members:
//   def getReference(): String (defined in class AtomicMarkableReference) and
//   def getReference: String (defined in trait Ref1)
//   (note: this can be resolved by declaring an `override` in class X.)

class Mark1a  extends AMR[String]("", false) with Ref1           { override def getReference   = "1a" }
class Mark1b  extends AMR[String]("", false) with Ref1           { override def getReference() = "1b" }

class Mark2a  extends AMR[String]("", false) with Ref2           { override def getReference   = "2a" }
class Mark2b  extends AMR[String]("", false) with Ref2           { override def getReference() = "2b" }

class Mark12a extends AMR[String]("", false) with Ref1 with Ref2 { override def getReference   = "12a" }
class Mark12b extends AMR[String]("", false) with Ref1 with Ref2 { override def getReference() = "12b" }

class Mark21a extends AMR[String]("", false) with Ref2 with Ref1 { override def getReference   = "21a" }
class Mark21c extends AMR[String]("", false) with Ref2 with Ref1 { override def getReference() = "21b" }
