package kit

/**
  * Routines for creating a minimum spanning tree for undirected, weighted graphs
  */
object MST {

  import collection.mutable.{PriorityQueue => MPQueue}
  import collection.mutable.ListBuffer

  /** Construct a MST using a Lazy version of Prim's algorithm
    *
    * If G is not connected, then the MST for the connected component
    * starting at vertex 0 will be returned
    */
  def LazyPrimMST(N: Int, adj: Int => Iterable[Int], weight: (Int, Int) => Double): (Double, ListBuffer[(Int, Int, Double)]) = {
    val edges = new MPQueue[((Int, Int), Double)]()(Ordering.by(_._2))
    val marked = Array.fill[Boolean](N)(false)
    val mst = new ListBuffer[(Int, Int, Double)]()

    def visit(u: Int): Unit = {
      marked(u) = true
      for (v <- adj(u))
        if (!marked(v)) edges += (((u, v), weight(u, v)))
    }

    visit(0)
    while (edges.nonEmpty) {
      val ((u, v), w) = edges.dequeue
      if (!(marked(u) && marked(v))) {
        mst += ((u, v, w))
        if (!marked(u)) visit(u)
        if (!marked(v)) visit(v)
      }
    }

    // Build return values
    val totwt = mst.foldLeft(0.0)(_ + _._3)
    (totwt, mst)
  }
}

