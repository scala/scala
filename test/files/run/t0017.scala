object Test extends App {

def transpose[A](arr: Array[Array[A]]) = {
  for (i <- Array.range(0, arr(0).length)) yield
    for (row <- arr) yield row(i)
}

var my_arr = Array(Array(1,2),Array(3,4))

for (i <- Array.range(0, my_arr(0).length)) yield
  for (row <- my_arr) yield row(i)

val transposed = transpose(my_arr)

println(transposed.deep.toString)

}
