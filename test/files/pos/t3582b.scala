object ParamScoping {
  // scoping worked fine in the result type, but was wrong in body
  // reason: typedTypeDef needs new context, which was set up by typed1 but not by typedDefDef and typedClassDef
  def noOverlapFOwithHO[T, G[T]]: G[T] =  null.asInstanceOf[G[T]]
}