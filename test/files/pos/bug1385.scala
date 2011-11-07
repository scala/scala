@serializable object Test {                    
  private def readResolve:AnyRef = this
}
