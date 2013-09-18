object boop {
  abstract class Bar { protected def f(): Any }
  class Bar1 extends Bar { protected def f(): Int = 5 }
  class Bar2 extends Bar { protected def f(): Int = 5 }

  val xs = List(new Bar1, new Bar2)

  type BarF = { def f(): Int }

  var x: BarF = _
  x = xs.head
  x.f

  (new Bar1).f
}
