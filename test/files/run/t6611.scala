object Test extends App {
  locally {
    val a = Array("1")
    val a2 = Array(a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array("1": Object)
    val a2 = Array(a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(true)
    val a2 = Array(a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(1: Short)
    val a2 = Array(a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(1: Byte)
    val a2 = Array(a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(1)
    val a2 = Array(a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(1L)
    val a2 = Array(a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(1f)
    val a2 = Array(a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(1d)
    val a2 = Array(a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(())
    val a2 = Array(a: _*)
    assert(a ne a2)
  }
}
