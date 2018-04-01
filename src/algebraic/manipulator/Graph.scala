package algebraic.manipulator

object Graph {
  def topologicalSort[K, T](map: Map[K, T], edges: (K, T) => Set[K]): List[(K, T)] = {
    class Node(val prod: (K, T), var edges: Set[Node] = Set.empty, var invEdges: Set[Node] = Set.empty)

    val lookup = map.map{case (k, t) => k -> new Node(k -> t)}

    for (n <- lookup.values) {
      n.edges = edges(n.prod._1, n.prod._2).map(p => try{lookup(p)} catch {case e: Throwable => throw new IllegalArgumentException(s"Missing dependency $p for ${n.prod}", e)})
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
