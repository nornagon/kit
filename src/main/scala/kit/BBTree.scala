package kit

class BBTree {

}

object BBTree {
  class Node private (
    var obj: Any,
    var bb: AABB,
    var parent: Node,
  ) {
    private [kit] var A: Node = _
    private [kit] var B: Node = _

    def setA(n: Node): Unit = {
      A = n
      n.parent = this
    }

    def setB(n: Node): Unit = {
      B = n
      n.parent = this
    }

    def isLeaf: Boolean = obj != null
    def other(n: Node): Node = if (n eq A) B else A

    def replaceChild(child: Node, newChild: Node): Unit = {
      assert(!isLeaf, "Cannot replace child of a leaf")
      assert((child eq A) || (child eq B), "Node is not a child of parent")
      if (child eq A)
        setA(newChild)
      else
        setB(newChild)

      var n = this
      while (n != null) {
        n.bb = n.A.bb + n.B.bb
        n = n.parent
      }
    }

    def query(qbb: AABB, cb: Any => Unit): Unit = {
      if (Intersections.intersects(bb, qbb)) {
        if (isLeaf) {
          cb(obj)
        } else {
          A.query(qbb, cb)
          B.query(qbb, cb)
        }
      }
    }
  }

  def proximity(a: AABB, b: AABB): Double = {
    (a.lower.x + a.upper.x - b.lower.x - b.upper.x).abs + (a.lower.y + a.upper.y - b.lower.y - b.upper.y).abs
  }

  private def subtreeInsert(subtree: Node, leaf: Node): Node = {
    if (subtree == null) {
      leaf
    } else if (subtree.isLeaf) {
      Node(leaf, subtree)
    } else {
      var cost_a = subtree.B.bb.area + (subtree.A.bb + leaf.bb).area
      var cost_b = subtree.A.bb.area + (subtree.B.bb + leaf.bb).area
      if (cost_a == cost_b) {
        cost_a = proximity(subtree.A.bb, leaf.bb)
        cost_b = proximity(subtree.B.bb, leaf.bb)
      }

      if (cost_b < cost_a)
        subtree.setB(subtreeInsert(subtree.B, leaf))
      else
        subtree.setA(subtreeInsert(subtree.A, leaf))

      subtree.bb += leaf.bb

      subtree
    }
  }

  private def subtreeRemove(subtree: Node, leaf: Node): Node = {
    if (leaf == subtree) {
      null
    } else {
      val parent = leaf.parent
      if (parent == subtree) {
        val other = subtree.other(leaf)
        other.parent = subtree.parent
        other
      } else {
        parent.parent.replaceChild(parent, parent.other(leaf))
        subtree
      }
    }
  }

  object Node {
    private [kit] def apply(a: Node, b: Node): Node = {
      val n = new Node(
        obj = null,
        bb = a.bb + b.bb,
        parent = null
      )
      n.setA(a)
      n.setB(b)
      n
    }
  }

  class Leaf private (
    obj: Any,
    parent: Node,
    stamp: Int = 1,
    pairs: Nothing,
  )
}
