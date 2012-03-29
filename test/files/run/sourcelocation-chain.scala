import reflect.SourceLocation

object Test extends App {

  /*
   * We'd like to get the SourceLocation of each invocation of
   * this method.
   */
  def userFacingOp(x: Int)(implicit loc: SourceLocation) {
    // internalOp() // WRONG
    // we cannot just call `internalOp` here, because it would pass `loc`
    // as the implicit argument to `internalOp`; therefore, we'd not
    // get the SourceLocation of the invocation of _internalOp_, but
    // the SourceLocation of the invocation of _userFacingOp_.
    //
    // The remedy is to shadow `loc`:
    val prev = implicitly[SourceLocation]
    println(prev.line) // should print 34
    
    {
      val loc: SourceLocation = null
      internalOp(prev)
    }
  }
  
  /*
   * This method is internal to the implementation of the DSL/library.
   * We'd still like to get the SourceLocation of each invocation.
   */
  def internalOp(prev: SourceLocation)(implicit loc: SourceLocation) {
    println(loc.line) // should print 22
  }
  
  userFacingOp(7)

}
