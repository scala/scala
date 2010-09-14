trait C

class Outer {
  object O0 extends C {}
  object O extends C { self => }
}