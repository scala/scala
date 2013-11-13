package test

class C {
  def fCCC : Int = 0
}

object O extends C {
  def fOOO : Int = 0
}

class Foo {
  {
    val o = O
    import o._
    /*_*/
  }
  {
    import O._
    /*_*/
  }
  {
    val c = new C
    import c._
    /*_*/
  }
  {
    f/*_*/
    val c = new C
    import c._
    f/*_*/
    import O._
    f/*_*/
  }
}

class Foo_1 {

  import O._

  def bar {
    /*_*/
  }
}

class Foo_2 {

  val o = O
  import o._

  def bar {
    /*_*/
  }
}

class Foo_3 {

  val c = new C
  import c._

  def bar {
    /*_*/
  }
}

