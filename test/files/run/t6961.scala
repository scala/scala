


import java.io._



object Test {

  def deepCopy[T](obj : T): T = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(obj)
    oos.flush()
    val data = baos.toByteArray
    val bais = new ByteArrayInputStream(data)
    val ois = new ObjectInputStream(bais)
    ois.readObject().asInstanceOf[T]
  }
 
  def test1() {
    var r: List[Int] = Nil
    var i = 0
    while (i < 1000000) {
      r = i :: r
      i += 1
    }
    assert(Nil == deepCopy(Nil))
    assert(r.take(1) == deepCopy(r.take(1)))
    assert(List(1, 2, 3) == deepCopy(List(1, 2, 3)))
    assert(r == deepCopy(r))
    val nestedList = r :: r
    assert(nestedList == deepCopy(nestedList))
  }
 
  def test2() {
    val l1: List[String] = "1" :: Nil
    val l2: List[String] = "2" :: "3" :: l1
    val p = (l1, l2)
    val pr = (l2, l1)
    val a = Array(l2, p)
    val l3: List[AnyRef] = p :: l2 :: l1 :: p :: a :: Nil
 
    def verifyL1L2(l1: List[String], l2: List[String]) {
      assert(l1 eq l2.tail.tail)
    }
 
    def verifyP(p: (List[String], List[String])) {
      verifyL1L2(p._1, p._2)
    }
 
    def verifyPr(p: (List[String], List[String])) {
      verifyL1L2(p._2, p._1)
    }
 
    def verifyL3(list: List[AnyRef]) {
      val p = list.head.asInstanceOf[(List[String], List[String])]
      val l1 = p._1
      val l2 = p._2
      val a = list.tail.tail.tail.tail.head.asInstanceOf[Array[AnyRef]]
      verifyP(p)
      assert(p eq list.tail.tail.tail.head)
      assert(l2 eq list.tail.head)
      assert(l1 eq list.tail.tail.head)
      assert(a(0) eq l2)
      assert(a(1) eq p)
    }
 
    verifyP(p)
    val pc = deepCopy(p)
    verifyP(pc)
 
    verifyPr(pr)
    val prc = deepCopy(pr)
    verifyPr(prc)
 
    verifyL3(l3)
    val l3c = deepCopy(l3)
    verifyL3(l3c)
  }
 
  def main(args: Array[String]) {
    test1()
    test2()
  }

}












