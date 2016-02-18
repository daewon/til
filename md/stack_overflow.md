Q: How to join two lists in Scala? Which is the best method to Join A and B to get

given
```
val a:List[String, Int] = List(("apple", 3),("orange", 4))
val b:List[String, String] = List(("mango", "25"),("orange", "50"))
```
expected

```
c: List[String, Int, String] = List("orange", 4, "50"))
```

A: Iterate over the first list and lookup values of the second list in a map `mb`.
The `flatMap` makes the entry disappear, if the `.get` returns `None`

```
val mb = b.toMap
a.flatMap { case (ka,va) => mb.get(ka).map(kb => (ka, va, kb)) }
```


Q: I have a use case where i need to return a String up to a delimiter String
(If found) from an iterator of Char

The contact:
    - if iterator is exhausted (only at the begin), return None
    - if the delimiter String is found, return all characters before it
      (empty String is fine), delimiter will be dropped
    - else return the remaining characters
    - do not eagerly exhaust the iterator!

I do have this working solution, but it feels like Java(which is where i'm coming from)


Is there a way I can do this in a more functional way (ideally without changing the method signature?)

Q: Sometime I stumble into the semi-mysterious notation of

```
def f[T](..) = new T[({type l[A]=SomeType[A, ..]})#l] {..}
```

Q: In Scala blog posts, which give it a "we used that type-lambda trick' handwave

While I have some intutition about this
(we gain an anonymous type parameter A without having to pollute the definition with it?)

I found no clear source describing what the type lambda trick is, and what are its benefits.
Is it just syntactic sugar, or does it open some new dimensions?


A: Type lambdas are vital quite a bit of the time when you are working withe higher-kinded types.

Consider a simple example of defining a monad for the right projection of `Eigher[A, B]`.

The monad typeclass looks like this.

```
trait Monad[M[_]] {
    def point[A](a: A): M[A]
    def bind[A, B](a: M[A])(f: M[A] => M[B]): M[B]
}
```
Now, Eigher is type constructor of two arguments,
but to implement Monad, You need to give it a type constructor of one argument.

The solution to this is to use a type lambda.

```
class EitherMonad[A] extends Monad[({type lm[a] = Either[A, ml]})#L] {
    def point[B](b: B): Either[A, B]
    def bind(B, C](m: Either[A, B])(f: B => Either[A, C]): Eigher[A, C]
}
```

This is an example of currying in the type system - you have curried the type of Either,
such that when you want to create an instacne of EitherMonad, you have to specify one of the types;
the other of course is supplied at the time you call point or bind.
