package funsets

object FunSets {
  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)
  def singletonSet(elem: Int): Set = x => x == elem
  def emptySet(): Set = x => false
  def union(s: Set, t: Set): Set =  x => s(x) || t(x)
  def intersect(s: Set, t: Set): Set = x => s(x) && t(x)
  def diff(s: Set, t: Set): Set = x => s(x) && !t(x)
  def filter(s: Set, p: Int => Boolean): Set = x => s(x) && p(x)

  val bound = 1000

  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a) && !p(a)) false
      else iter(a+1)
    }
    iter(-bound)
  }

  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) false
      else if (s(a) && p(a)) true
      else iter(a+1)
    }
    iter(-bound)
  }

  def map(s: Set, f: Int => Int): Set = {
    def iter(a: Int, prevSet: Set): Set = {
      if (a > bound) prevSet
      else if (!s(a)) iter(a+1, prevSet)
      else iter(a+1, union(prevSet, singletonSet(f(a))))
    }
    iter(-bound, emptySet())
  }

  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  def printSet(s: Set) {
    println(toString(s))
  }
}
