package scala.math

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class BigIntRSABenchmark {

  @Benchmark
  def encodeDecode(bh: Blackhole): Unit = {
    // private key
    val d = BigInt("5617843187844953170308463622230283376298685")
    // public key
    val n = BigInt("9516311845790656153499716760847001433441357")
    val e = 65537

    // concatenation of "Scala is great"
    val plaintext = BigInt("83099097108097032105115032103114101097116")
    val ciphertext = plaintext.modPow(e, n)
    val recoveredtext = ciphertext.modPow(d, n)
    bh.consume(plaintext == recoveredtext)
  }

}
