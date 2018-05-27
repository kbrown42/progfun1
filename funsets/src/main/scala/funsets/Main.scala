package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1)) // true
  val x = singletonSet(1)
  val t = singletonSet(2)
  val u = union(x, t)
  println(contains(u, 3)) // false
  printSet(u)
  val m = map(u, x => x * 2)
  println(contains(m, 4))

}
