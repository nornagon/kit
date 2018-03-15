package kit.pcg

import kit.{AABB, Rand, Vec2}


object LloydRelaxation {
  /**
    * Perform Lloyd relaxation by Monte Carlo approximation.
    *
    * Size of output not guaranteed to be same as size of input.
    *
    * @see https://en.wikipedia.org/wiki/Lloyd%27s_algorithm
    * @param bounds bounding box containing all points
    * @param points set of points to relax
    * @return a set of points that are a little more relaxed B)
    */
  def monteCarloRelax(bounds: AABB, points: Iterable[Vec2]): Iterable[Vec2] = {
    // 1. generate some number of random points within `bounds`
    val numPoints = points.size * 5
    val samples = (1 to numPoints) map { _ => Rand.withinAABB(bounds) }
    // 2. group the generated points by the member of `points` that's closest to each
    val groups = samples.groupBy { p =>
      points.minBy { o =>
        val dx = p.x - o.x
        val dy = p.y - o.y
        dx * dx + dy * dy
      }
    }
    // 3. calculate the centroid of each group
    groups.values.map { ss =>
      ss.reduce(_ + _) / ss.size
    } ++ (points.toSet -- groups.keySet)
  }

  def voronoiRelax(bounds: AABB, points: Iterable[Vec2]): Iterable[Vec2] = {
    /*val v = new Voronoi(points.toSeq)
    while (!v.step()) ()
    for ((_, cell) <- v.cells()) yield cell.centroid*/
    /*
    val polygons = Voronoi.computeD3(points)
    for (p <- polygons) yield p.centroid
    */
    ???
  }
}
