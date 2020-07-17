object C {
  private class Inner

  class OtherInner {
    new Inner // trigger makeNotPrivate of `Inner`.
  }
}
