object Test extends App {
  locally {
    val a = Array("1")
    val a2 = Array(a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array("1": Object)
    val a2 = Array[Object](a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(true)
    val a2 = Array[Boolean](a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(1: Short)
    val a2 = Array[Short](a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(1: Byte)
    val a2 = Array[Byte](a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(1)
    val a2 = Array[Int](a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(1L)
    val a2 = Array[Long](a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(1f)
    val a2 = Array[Float](a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(1d)
    val a2 = Array[Double](a: _*)
    assert(a ne a2)
  }

  locally {
    val a = Array(())
    val a2 = Array[Unit](a: _*)
    assert(a ne a2)
  }
}
