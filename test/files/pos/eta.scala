object test {

def sum(f: Int => Int)(x: Int, y: Int): Int = 0;
def g: (Int => Int) => (Int, Int) => Int = sum;
}
