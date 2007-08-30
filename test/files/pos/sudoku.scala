object SudokuSolver extends Application {
  // The board is represented by an array of strings (arrays of chars),
  // held in a global variable m. The program begins by reading 9 lines
  // of input to fill the board
  var m: Array[Array[char]] = List.tabulate( 9, (x:int) => Console.readLine.toArray).toArray;

  // For printing m, a method print is defined
  def print = {Console.println(""); m map (carr=>Console.println(new String(carr)))}

  // The test for validity is performed by looping over i=0..8 and
  // testing the row, column and 3x3 square containing the given
  // coordinate
  def invalid(i:int, x:int, y:int, n:char): boolean =
    i<9 && (m(y)(i) == n || m(i)(x) == n ||
      m(y/3*3 + i/3)(x/3*3 + i % 3) == n || invalid(i+1, x, y, n))

  // Looping over a half-closed range of consecutive integers [l..u)
  // is factored out into a higher-order function
  def fold(f: (int,int)=>int, accu:int, l:int, u:int):int =
    if(l==u) accu else fold(f, f(accu, l), l+1, u)

  // The search function examines each position on the board in turn,
  // trying the numbers 1..9 in each unfilled position
  // The function is itself a higher-order fold, accumulating the value
  // accu by applying the given function f to it whenever a solution m
  // is found
  def search(x:int, y:int, f:(int)=>int, accu:int):int = Pair(x, y) match {
    case Pair(9, y) => search(0, y+1, f, accu) // next row
    case Pair(0, 9) => f(accu)                 // found a solution
    case Pair(x, y) => if(m(y)(x) != '0') search(x+1, y, f, accu) else
      fold((accu:int, n:int) =>
           if(invalid(0, x, y, (n + 48).toChar)) accu else {
             m(y)(x) = (n + 48).toChar;
             val newaccu = search(x+1, y, f, accu);
             m(y)(x) = '0';
             newaccu}, accu, 1, 10)}

  // The main part of the program uses the search function to accumulate
  // the total number of solutions
  Console.println("\n"+search(0,0,i => {print; i+1},0)+" solution(s)")
}
