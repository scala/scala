package hoho

class G

class H extends G

class A[T](x: T) {

  def this(y: G, z: T) = {
    this(z)
    print(1)
  }

  def this(z: H, h: T) = {
    this(h)
    print(2)
  }
}

object T {
  def main(args: Array[String]) {
    implicit def g2h(g: G): H = new H
    new A(new H, 23)
  }
}