package kit

import scala.collection.mutable

class GridIndex[T](cellSize: Double, pos: T => Vec2) {
  def foreach(f: T => Unit): Unit = {
    for (gs <- grid.values; g <- gs) f(g)
  }

  private val grid = mutable.HashMap.empty[(Int, Int), mutable.Set[T]]
  def insert(t: T): Unit = {
    val Vec2(x, y) = pos(t) / cellSize
    val gx = x.floor.toInt
    val gy = y.floor.toInt
    val s = grid.getOrElseUpdate((gx, gy), mutable.Set.empty)
    s.add(t)
  }
  def query(bb: AABB)(f: T => Unit): Unit = {
    val lx = (bb.lower.x / cellSize).floor.toInt
    val ly = (bb.lower.y / cellSize).floor.toInt
    val ux = (bb.upper.x / cellSize).ceil.toInt
    val uy = (bb.upper.y / cellSize).ceil.toInt
    for (x <- lx to ux; y <- ly to uy; gs <- grid.get((x, y)); g <- gs) f(g)
  }
}
