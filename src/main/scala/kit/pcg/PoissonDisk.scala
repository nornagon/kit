package kit.pcg

import kit.RandomImplicits._
import kit.{AABB, Vec2}

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Random

/**
  * Fast Poisson Disk Sampling in Arbitrary Dimensions, by Robert Bridson
  * http://www.cs.ubc.ca/~rbridson/docs/bridson-siggraph07-poissondisk.pdf
  *
  * See also:
  * - https://bl.ocks.org/mbostock/dbb02448b0f93e4c82c3, a visualization
  * - http://devmag.org.za/2009/05/03/poisson-disk-sampling/, some applications and modifications
  * - http://www.redblobgames.com/articles/noise/introduction.html, where I discovered this technique
  * - http://www.cs.virginia.edu/~gfx/pubs/antimony/antimony.pdf, "A spatial data structure for fast poisson-disk sample generation"
  *   Daniel Dunbar & Greg Humphreys, UVa
  * - http://nkhademi.com/Papers/EGSR12_FastApproximateBlueNoise.pdf for a possible way of approaching unbounded noise
  */
object PoissonDisk {
  private val MaxPlacementAttempts = 16

  class Grid2[T: ClassTag] private (w: Int, h: Int, val cells: Array[T]) {
    def this(w: Int, h: Int) = this(w, h, new Array[T](w * h))
    def get(x: Int, y: Int): T = if (x >= 0 && x < w && y >= 0 && y < h) cells(y * w + x) else null.asInstanceOf[T]
    def get(p: (Int, Int)): T = get(p._1, p._2)
    def set(x: Int, y: Int, v: T): Unit = if (x >= 0 && x < w && y >= 0 && y < h) cells(y * w + x) = v else throw new IndexOutOfBoundsException(s"($x, $y) was outside grid of size ${w}x$h")
    def set(p: (Int, Int), v: T): Unit = set(p._1, p._2, v)
  }
  object Grid2 {
    def fill[T: ClassTag](w: Int, h: Int)(elem: => T): Grid2[T] = new Grid2(w, h, Array.fill(w * h)(elem))
  }
  def generate(domain: AABB, minDist: Double, maxPlacements: Int = 50000, maxPlacementAttempts: Int = MaxPlacementAttempts)(implicit r: Random): Seq[Vec2] = {
    val gridCellSize = minDist / math.sqrt(2)
    val gridW = (domain.width / gridCellSize).ceil.toInt
    val gridH = (domain.height / gridCellSize).ceil.toInt
    val grid = new Grid2[Vec2](gridW, gridH)
    val active = mutable.ArrayBuffer.empty[Vec2]

    implicit def vecToGrid(v: Vec2): (Int, Int) = {
      (((v.x - domain.lower.x) / gridCellSize).toInt, ((v.y - domain.lower.y) / gridCellSize).toInt)
    }

    val x0 = r.withinAABB(domain)
    grid.set(x0, x0)
    active.append(x0)

    def canPlace(placement: Vec2): Boolean = {
      if (!domain.contains(placement))
        return false
      val gridX = ((placement.x - domain.lower.x) / gridCellSize).toInt
      val gridY = ((placement.y - domain.lower.y) / gridCellSize).toInt
      // grrrr performance
      var offX = -1
      while (offX <= 1) {
        var offY = -1
        while (offY <= 1) {
          val p = grid.get(gridX + offX, gridY + offY)
          if (p != null && {
            val pdx = p.x - placement.x
            val pdy = p.y - placement.y
            (pdx*pdx+pdy*pdy) < minDist*minDist
          }) return false
          offY += 1
        }
        offX += 1
      }
      true
    }

    val max = math.min(gridW * gridH, maxPlacements)
    var numPlacements = 0
    while (active.nonEmpty && numPlacements < max) {
      val activeIdx = r.between(0, active.size)
      val point = active(activeIdx)
      (1 to maxPlacementAttempts).view map (_ => point + r.withinAnnulus(minDist, minDist * 2)) find canPlace match {
        case Some(p) =>
          grid.set(p, p)
          active.append(p)
        case None =>
          active(activeIdx) = active(active.size - 1)
          active.remove(active.size - 1, 1)
      }
      numPlacements += 1
    }

    grid.cells.filter(_ != null)
  }

  def generateModulated(domain: AABB, minDist: Vec2 => Double, maxMinDist: Double, maxPlacements: Int = 50000, maxPlacementAttempts: Int = MaxPlacementAttempts)(implicit r: Random): Seq[Vec2] = {
    val gridCellSize = maxMinDist / math.sqrt(2)
    val gridW = (domain.width / gridCellSize).ceil.toInt
    val gridH = (domain.height / gridCellSize).ceil.toInt
    val grid = Grid2.fill(gridW, gridH)(mutable.Buffer.empty[Vec2])
    val active = mutable.ArrayBuffer.empty[Vec2]

    implicit def vecToGrid(v: Vec2): (Int, Int) = {
      (((v.x - domain.lower.x) / gridCellSize).toInt, ((v.y - domain.lower.y) / gridCellSize).toInt)
    }

    val x0 = r.withinAABB(domain)
    grid.get(x0).append(x0)
    active.append(x0)

    def canPlace(placement: Vec2): Boolean = {
      if (!domain.contains(placement))
        return false
      val gridX = ((placement.x - domain.lower.x) / gridCellSize).toInt
      val gridY = ((placement.y - domain.lower.y) / gridCellSize).toInt
      val minDistHere = minDist(placement)
      assert(minDistHere > 0)
      // grrrr performance
      var offX = -1
      while (offX <= 1) {
        var offY = -1
        while (offY <= 1) {
          val ps = grid.get(gridX + offX, gridY + offY)
          if (ps != null && {
            ps.exists(p => {
              val pdx = p.x - placement.x
              val pdy = p.y - placement.y
              (pdx*pdx+pdy*pdy) < minDistHere*minDistHere
            })
          }) return false
          offY += 1
        }
        offX += 1
      }
      true
    }

    val max = maxPlacements
    var numPlacements = 0
    while (active.nonEmpty && numPlacements < max) {
      val activeIdx = r.between(0, active.size)
      val point = active(activeIdx)
      val minDistHere = minDist(point)
      assert(minDistHere > 0, s"minDist must be > 0, but was $minDistHere")
      (1 to maxPlacementAttempts).view map (_ => point + r.withinAnnulus(minDistHere, minDistHere * 2)) find canPlace match {
        case Some(p) =>
          grid.get(p).append(p)
          active.append(p)
        case None =>
          active(activeIdx) = active(active.size - 1)
          active.remove(active.size - 1, 1)
      }
      numPlacements += 1
    }

    grid.cells.filter(_ != null).toSeq.flatten
  }
}
