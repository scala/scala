trait Event

trait Domain {
  case class Created(name: String) extends Event
}

// declare three instances of Domain trait, one here and two
// in an inner scope

object DomainC extends Domain

object Test {
 def main(args: Array[String]) {
   object DomainA extends Domain
   object DomainB extends Domain

   def lookingForAs(event: Event): Unit = {
      event match {
        case DomainB.Created(_) => throw null
        case DomainC.Created(_) => throw null
        case DomainA.Created(_) => // okay
      }
   }

   lookingForAs(DomainA.Created("I am an A"))
  }
}
