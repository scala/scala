object UseParent {
  classOf[Parent[AnyRef]#I2]

  // OKAY
  classOf[Parent[AnyRef]#I3]
  classOf[Parent.I5]
}
