package c

import com.google.common.io.Files
import org.apache.commons.lang._

case class B(int: Int) {

	def test = Files.asCharSource(null, null).readLines()
	def test2 = BooleanUtils.isTrue(false)
}