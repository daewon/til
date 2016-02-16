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
