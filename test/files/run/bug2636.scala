object Test
{
  type Foo = { def update(x: Int, value: String): Unit }
  type Foo2 = { def update(x: Int, value: String): Int }
  type Foo3 = { def update(x: Int, value: String): Array[Int] }
  
  def alen() = {
    type L1 = { def length: Int }
    def len(p: L1) = p.length
    val x: L1 = Array(1,2,3)
    len(x)
  }
  
  type A1 = { def apply(x: Int): String }
  def arrApply(a: A1, x: Int) = a(x)
  
  def main(args: Array[String]): Unit = {
    val arr = new Array[String](3)
    val p1: Foo = arr
    def a1 = p1(0) = "b"

    val p2: Foo2 = new { def update(x: Int, value: String) = { p1(1) = "o" ; 1 } }
    def a2 = p2(0) = "c"    
    
    val p3: Foo3 = new { def update(x: Int, value: String) = { p1(2) = "b" ; Array(1) } }
    def a3 = p3(10) = "hi mom"
    
    a1 ; a2 ; a3 ;

    assert(arr.mkString == "bob")
    assert(alen() == 3)
    assert(arrApply(arr, 1) == "o")
    assert(arrApply(new { def apply(x: Int) = "tom" }, -100) == "tom")
  }
}