class Trac4366 {
  /**
   * <strong><code>foo</code> has been deprecated and will be removed in a future version of
   * ScalaTest. Please call <code>bar</code> instead.</strong>
   */
  @deprecated // deprecated in 1.0, remove in 1.4
  val foo: Option[String] = None
}
