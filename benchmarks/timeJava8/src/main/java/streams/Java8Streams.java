package streams;

import java.util.stream.*;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;

import java.util.concurrent.TimeUnit;
import java.util.Random;

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Mode.AverageTime)
@Fork(15)
@Warmup(iterations = 30)
@Measurement(iterations = 15)
@State(Scope.Benchmark)
public class Java8Streams {

    @Param({"39", "282", "73121", "7312102"})
    public int size;

    @Param({"39"})
    public int vLoSize;

    public int shortRangingFactor = (int)(size * 0.2);

    public Long[] v_B, vLo_B;

    public long[] v_P, vLo_P;

    public Long[] fillArray_B(int range){
        Random r = new Random();
        Long[] array = new Long[range];
        for (int i = 0; i < range; i++) {
            array[i] = (new Long(r.nextInt(size)));
        }
        return array;
    }

    public long[] fillArray_P(int range){
        Random r = new Random();
        long[] array = new long[range];
        for (int i = 0; i < range; i++) {
            array[i] = ((long) r.nextInt(size));
        }
        return array;
    }

    @Setup(Level.Trial)
    public void initData() {
        v_B  = fillArray_B(size);
        vLo_B = fillArray_B(vLoSize);

        v_P = fillArray_P(size);
        vLo_P = fillArray_P(vLoSize);
    }

    @Benchmark
    public void sum_primitive(Blackhole bh) {
        long ret = LongStream.of(v_P)
                .reduce(0, (a, b) -> a + b);

        bh.consume(ret);
    }

    @Benchmark
    public void sumOfSquares_primitive(Blackhole bh) {
        long ret = LongStream.of(v_P)
                .map(d -> d * d)
                .reduce(0, (a, b) -> a + b);
        bh.consume(ret);
    }

    @Benchmark
    public void sumOfSquaresEven_primitive(Blackhole bh) {
        long ret = LongStream.of(v_P)
                .filter(x -> x % 2 == 0)
                .map(x -> x * x)
                .reduce(0, (a, b) -> a + b);
        bh.consume(ret);
    }

    @Benchmark
    public void cart_primitive(Blackhole bh) {
        long ret = LongStream.of(v_P)
                .flatMap(d -> LongStream.of(vLo_P)
                    .map(dP -> dP * d))
                .reduce(0, (a, b) -> a + b);
        bh.consume(ret);
    }

    @Benchmark
    public void maps_primitive(Blackhole bh) {
        long ret = LongStream.of(v_P)
                .map(x -> x + (x & 0xD) + 0xCAFED00D)
                .map(x -> x + (x & 0xE) + 0xD15EA5E)
                .map(x -> x + (x & 0xA) + 0xDABBAD00)
                .reduce(0, (a, b) -> a + b);
        bh.consume(ret);
    }

    @Benchmark
    public void filters_primitive(Blackhole bh) {
        long ret = LongStream.of(v_P)
                .filter(x -> (x & 0x13) != 0x11)
                .filter(x -> (x & 0x12) == 0x12)
                .filter(x -> (x & 0x11) != 0x10)
                .reduce(0, (a, b) -> a + b);
        bh.consume(ret);
    }

    @Benchmark
    public void flatMap_take_primitive(Blackhole bh) {
        long ret = LongStream.of(v_P)
                .flatMap(x -> LongStream.of(vLo_P)
                        .map(dP -> dP * x))
                .limit(shortRangingFactor)
                .reduce(0, (a, b) -> a + b);
        bh.consume(ret);
    }

    @Benchmark
    public void sum_boxed(Blackhole bh) {
        Long ret = Stream.of(v_B)
                .reduce(0L, (a, b) -> a + b);

        bh.consume(ret);
    }

    @Benchmark
    public void sumOfSquares_boxed(Blackhole bh) {
        Long ret = Stream.of(v_B)
                .map(d -> d * d)
                .reduce(0L, (a, b) -> a + b);
        bh.consume(ret);
    }

    @Benchmark
    public void sumOfSquaresEven_boxed(Blackhole bh) {
        Long ret = Stream.of(v_B)
                .filter(x -> x % 2 == 0)
                .map(x -> x * x)
                .reduce(0L, (a, b) -> a + b);
        bh.consume(ret);
    }

    @Benchmark
    public void cart_boxed(Blackhole bh) {
        Long ret = Stream.of(v_B)
                .flatMap(d -> Stream.of(vLo_B)
                        .map(dP -> dP * d))
                .reduce(0L, (a, b) -> a + b);
        bh.consume(ret);
    }

    @Benchmark
    public void maps_boxed(Blackhole bh) {
        Long ret = Stream.of(v_B)
                .map(x -> x + (x & 0xD) + 0xCAFED00D)
                .map(x -> x + (x & 0xE) + 0xD15EA5E)
                .map(x -> x + (x & 0xA) + 0xDABBAD00)
                .reduce(0L, (a, b) -> a + b);
        bh.consume(ret);
    }

    @Benchmark
    public void filters_boxed(Blackhole bh) {
        Long ret = Stream.of(v_B)
                .filter(x -> (x & 0x13) != 0x11)
                .filter(x -> (x & 0x12) == 0x12)
                .filter(x -> (x & 0x11) != 0x10)
                .reduce(0L, (a, b) -> a + b);
        bh.consume(ret);
    }

    @Benchmark
    public void flatMap_take_boxed(Blackhole bh) {
        Long ret = Stream.of(v_B)
                .flatMap(x -> Stream.of(vLo_B)
                        .map(dP -> dP * x))
                .limit(shortRangingFactor)
                .reduce(0L, (a, b) -> a + b);
        bh.consume(ret);
    }
}