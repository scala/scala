// Tests detection of overlapping erased signatures

// $Id$

trait X {
  type I;
  type J;
  def foo: I;
  def foo: J;
}
