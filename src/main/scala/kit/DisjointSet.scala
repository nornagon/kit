package kit

import scala.collection.mutable

// Disjoint Set aka Union-Find
class DisjointSet[T] {
  private val parents = mutable.Map.empty[T, T]
  private val sizes = mutable.Map.empty[T, Int]

  def makeSet(n: T): Unit = {
    if (!contains(n)) {
      parents(n) = n
      sizes(n) = 1
    }
  }

  def contains(n: T): Boolean = parents contains n

  def find(n: T): Option[T] = {
    if (!contains(n)) return None
    var x = n
    while (parents(x) != x) {
      val parent = parents(x)
      parents(x) = parents(parent)
      x = parent
    }
    Some(x)
  }

  def union(u: T, v: T): Unit = {
    val xP = find(u)
    val yP = find(v)
    if (xP.isEmpty || yP.isEmpty) return
    if (xP == yP) return

    val (Some(x), Some(y)) = if (sizes(xP.get) < sizes(yP.get)) (yP, xP) else (xP, yP)
    parents(y) = x
    sizes(x) += sizes(y)
    sizes.remove(y)
    // TODO: more sizes cleanup?
  }
}
