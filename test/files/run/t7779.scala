// -Xmax-classfile-length doesn't compress top-level classes.
// class :::::::::::::::::::::::::::::::::::::::::::::::::

trait Marker

class Short extends Marker

// We just test with member classes
object O {
  object ::::::::::::::::::::::::::::::::::::::::::::::::: extends Marker
}
class C {
  class D {
    class ::::::::::::::::::::::::::::::::::::::::::::::::: extends Marker
  }
}

package pack {
  // abbreviates to: $colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon to $read$$iw$$iw$$colon$colon$colon$colon$colon$colon$colon$colon$$$$c39b3f245029fbed9732fc888d44231b$$$$on$colon$colon$colon$colon$colon$colon$colon$colon$colon$colon
  // class :::::::::::::::::::::::::::::::::::::::::::::::::

  class Short extends Marker

  // We just test with member classes
  object O {
    object ::::::::::::::::::::::::::::::::::::::::::::::::: extends Marker
  }
  class C {
    class D {
      class ::::::::::::::::::::::::::::::::::::::::::::::::: extends Marker
    }
  }
  package p2 {
    class Short extends Marker

    object O {
      object ::::::::::::::::::::::::::::::::::::::::::::::::: extends Marker
    }
    class C {
      class D {
        class ::::::::::::::::::::::::::::::::::::::::::::::::: extends Marker
      }
    }
  }
}


object Test extends App {
  import reflect.runtime.universe._
  def test[T: TypeTag] = {
    val tt = typeTag[T]
    val clz = tt.mirror.runtimeClass(tt.tpe)
    assert(classOf[Marker].isAssignableFrom(clz), clz.toString)
  }

  test[Short]
  test[O.:::::::::::::::::::::::::::::::::::::::::::::::::.type]
  test[C#D#`:::::::::::::::::::::::::::::::::::::::::::::::::`]

  test[pack.Short]
  test[pack.O.:::::::::::::::::::::::::::::::::::::::::::::::::.type]
  test[pack.C#D#`:::::::::::::::::::::::::::::::::::::::::::::::::`]

  test[pack.p2.Short]
  test[pack.p2.O.:::::::::::::::::::::::::::::::::::::::::::::::::.type]
  test[pack.p2.C#D#`:::::::::::::::::::::::::::::::::::::::::::::::::`]
}
