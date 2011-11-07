object Test extends App {
	def testTakeWhile = {
		val numbers = Iterator.range(0, 50)
		val zeroTo9 = numbers.takeWhile(x => { println("p(" + x + ")"); x < 10 } )
		
		zeroTo9.foreach(println _)
		
		val zeroTo1 = Iterator.range(0, 20).takeWhile(x => { println("p(" + x + ")"); x < 2 } )
	
		println(zeroTo1.hasNext)
		println(zeroTo1.hasNext)
		println(zeroTo1.next)
		println(zeroTo1.hasNext)
		println(zeroTo1.next)
		println(zeroTo1.hasNext)
		println(zeroTo1.hasNext)
	}
	
	def testFilter = {
		val predicate = (x: Int) => { println("p(" + x + ")"); x % 2 == 0 }
		
		val evens = Iterator.range(0, 10).filter(predicate)
		
		println(evens.hasNext)
		println(evens.hasNext)
		println(evens.next)
		
		evens.foreach(println _)
	}

	testTakeWhile
	testFilter
}

