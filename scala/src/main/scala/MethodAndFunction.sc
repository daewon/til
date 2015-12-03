/**
  * https://tpolecat.github.io/2014/06/09/methods-functions.html
  */
// Methods are not Functions
def add1(n: Int): Int = n + 1
// val f = add1 // error, method is not function
val f = add1 _ // convert method to function
f(3)
// Make Function instance using Function1 class
val g = new Function[Int, Int] {
  override def apply(v1: Int): Int = v1 + 1
}
g(3)
// Automatic Expansion
Seq(1, 2, 3) map add1
Seq(1, 2, 3) map add1 _
val z: Int => Int = add1
val z1 = add1: Int => Int
z(3)
z1(3)
// Effective Overloading
val one = "foo".substring _ : Int => String // one argument
val two = "foo".substring _ : (Int, Int) => String // two argument
one(3)
two(0, 3)
// Fistful of Parameters
def x = println("hi")
val xf = x _
// function cannot call without paren () but method can
xf()
// Multiple Parameter
def plus(a: Int, b: Int): Int = a + b
val plusf = plus _
plusf(3, 4)
def plus2(a: Int)(b: Int) = a + b
val plus2f = plus2 _
//plus2(3) // error method
plus2f(3) // no error function
// Type Parameters
def id[A](a: A) = a
val xid = id[Int] _ // must specify Type Parameter when transform method to function
xid(1)

// Implicit Parameters: implicit parameter is disappear
def foo[N: Numeric](n: N): N = n
foo(10)
val foof = foo[Int] _
foof(10)

def foo2[N](n: N)(implicit ev: Numeric[N]): N = n
val foo2f = foo2[Int] _
// By Name parameter
def fool(a: => Unit): Int = 10
fool(println("do not evaluated"))

def foolf = fool _
foolf(println("do not evaluated too"))

// Sequence Parameter
def seq(args: Int*) = args
seq(1, 2, 3)
def seqf = seq _
seqf(Seq(1, 2, 3))

// Default Parameter
def default(a: Int, s: String = "daewon") = s"$a -> $s"
default(10)
val defaultf = default _
// defaultf(10) // error, cannot work default parameter on function
defaultf(10, "ok")

