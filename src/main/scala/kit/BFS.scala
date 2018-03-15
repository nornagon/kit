package kit

import scala.collection.mutable

object BFS {
  def find[T](from: T, links: T => Seq[T], pred: T => Boolean): Option[T] = {
    path(from, links, pred).map(_.last)
  }

  def path[T](from: T, links: T => Iterable[T], pred: T => Boolean): Option[Seq[T]] = {
    if (pred(from))
      return Some(Seq(from))
    val q = mutable.Queue.empty[T]
    val visits = mutable.Map.empty[T, T]
    visits.update(from, from)
    q.enqueue(from)
    while (q.nonEmpty) {
      val e = q.dequeue()
      val ls = links(e)
      for (n <- ls) {
        if (n != from && !visits.contains(n)) {
          visits += n -> e
          if (pred(n)) {
            val path = mutable.Buffer[T](n)
            var x = n
            while (visits(x) != x) {
              val t = visits(x)
              path.append(t)
              x = t
            }
            return Some(path.reverse)
          }
          q.enqueue(n)
        }
      }
    }
    None
  }

  def reachableFrom[T](from: T, links: T => Seq[T]): Set[T] = {
    val q = mutable.Queue.empty[T]
    val visited = mutable.Set.empty[T]
    visited.add(from)
    q.enqueue(from)
    while (q.nonEmpty) {
      val e = q.dequeue()
      val ls = links(e)
      for (n <- ls) {
        if (!visited(n)) {
          visited.add(n)
          q.enqueue(n)
        }
      }
    }
    visited.toSet
  }

  def dijkstra[T](from: T, links: T => Iterable[(T, Double)]): (Map[T, Double], Map[T, T]) = {
    val active = mutable.Set.empty[T]
    val distance = mutable.Map.empty[T, Double] // distance to k from source
    val shortestApproach = mutable.Map.empty[T, T] // shortest approach to get to k from source
    distance += from -> 0
    active += from
    while (active.nonEmpty) {
      val a = active.minBy(distance)
      active.remove(a)
      for ((n, c) <- links(a)) {
        val alt = distance(a) + c
        if (!distance.contains(n) || alt < distance(n)) {
          distance(n) = alt
          shortestApproach(n) = a
        }
      }
    }
    (distance.toMap, shortestApproach.toMap)
  }

  def dijkstraShortest[T](from: T, links: T => Iterable[(T, Double)], pred: T => Boolean): Option[Seq[T]] = {
    val active = mutable.Set.empty[T]
    val distance = mutable.Map.empty[T, Double] // distance to k from source
    val shortestApproach = mutable.Map.empty[T, T] // shortest approach to get to k from source
    distance += from -> 0
    active += from
    while (active.nonEmpty) {
      val a = active.minBy(distance)
      active.remove(a)
      if (pred(a)) {
        val path = mutable.Buffer[T](a)
        var x = a
        while (shortestApproach.contains(x)) {
          val t = shortestApproach(x)
          path.append(t)
          x = t
        }
        return Some(path.reverse)
      }
      for ((n, c) <- links(a)) {
        val alt = distance(a) + c
        if (!distance.contains(n) || alt < distance(n)) {
          active += n
          distance(n) = alt
          shortestApproach(n) = a
        }
      }
    }
    None
  }
}