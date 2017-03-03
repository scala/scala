trait T {
  // Need to mark the synthesized member in the object's module class as notPROTECTED,
  // since the trait member will receive this flag later.
  // If we don't add notPROTECTED to the synthesized one, the member will not be seen as overriding the trait member.
  // Therefore, addForwarders's call to membersBasedOnFlags would see the deferred member in the trait,
  // instead of the concrete (desired) one in the class, and thus not create the static forwarder.
  protected val propFilename: String = "/"
}

object P extends T
