package algebraic.manipulator

object Graph {
  def topologicalSort[K, T](map: Map[K, T], edges: (K, T) => Set[K]): List[(K, T)] = {
    class Node(val prod: (K, T), var edges: Set[Node] = Set.empty, var invEdges: Set[Node] = Set.empty)

    val lookup = (Map.empty[K, Node] /: map)((lookup, p) => lookup + (p._1 -> new Node(p)))

    for (n <- lookup.values) {
      n.edges = edges(n.prod._1, n.prod._2).map(lookup)
      n.edges.foreach(e => e.invEdges += n)
    }

    var free = lookup.values.filter(_.invEdges.isEmpty).toList
    var result = List.empty[(K, T)]

    while (free.nonEmpty) {
      val head = free.head
      free = free.tail
      result ::= head.prod

      for (e <- head.edges) {
        e.invEdges -= head
        if (e.invEdges.isEmpty)
          free ::= e
      }
    }

    result
  }
}
