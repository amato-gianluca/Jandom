/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.domains.objects

import it.unich.jandom.utils.DisjointSets
import scala.annotation._
import scala.annotation.elidable._

/**
 * The domain for definite weak aliasing. Two identifiers are weak aliased if either they are
 * both null, or they point to the same location. This information is encoded in a graph called
 * aliasing graph.
 * @note this class mixes methods for the public interface of object domains with methods for low level
 * functionalities which should be factored out.
 * @author Gianluca Amato <gamato@unich.it>
 */
class AliasingDomain[OM <: ObjectModel](val om: OM) extends ObjectDomain[OM] {

  import AliasingDomain._

  /**
   * A Span is a set of labeled edges departing from the same source node, 
   * represented as a map from labels to target node. 
   */
  private[objects]type Span = Map[om.Field, Node]

  /**
   * An EdgeSet is a map from a node to its corresponding outer span.
   */
  private[objects]type EdgeSet = Map[Node, Span]

  /**
   * Returns a full span for a node of type `t`.
   */
  private[objects] def fullSpan(t: om.Type): Span = {
    (for { f <- om.fieldsOf(t); tf = om.typeOf(f); if om.mayShare(tf, tf) } yield f -> Node()) (collection.breakOut)
  }

  def top(types: Fiber) = {
    val labels = for { t <- types } yield if (om.mayShare(t, t)) Some(Node()) else None
    val edges: EdgeSet = (for { (t, Some(n)) <- types zip labels } yield n -> fullSpan(t)) (collection.breakOut)
    new Property(labels, edges, types)
  }

  def bottom(types: Fiber) =
    new Property(Seq.fill(types.size)(None), Map.empty, types)

  /**
   * Builds an aliasing graph with given labels, edges and, types. Edges are given in the form
   * of an EdgeSet, and they are completed by turning undefined spans into empty spans. 
   */
  private[objects] def apply(labels: Seq[Option[Node]], partialEdgeSet: EdgeSet, types: Seq[om.Type]): Property = {
    val edgeSet: EdgeSet = (
      for (Some(l) <- labels.toSet) yield if (partialEdgeSet.isDefinedAt(l))
        l -> partialEdgeSet(l)
      else
        l -> Map.empty[om.Field, Node] 
    )(collection.breakOut)
    new Property(labels, edgeSet, types)
  }

  /**
   * Builds an aliasing graph with given labels, edges and types. Edges are given as a sequence
   * of triples `(n1,f,n2)` where `n1` is the source node, `n2` is the target node and `f` the field name.
   */
  private[objects] def apply(labels: Seq[Option[Node]], edges: Seq[(Node, om.Field, Node)], types: Seq[om.Type]): Property = {
    val partialEdgeSet = edges.groupBy(_._1).
      mapValues(_.groupBy(_._2).
        mapValues(_.head._3))
    apply(labels, partialEdgeSet, types)
  }

  /**
   * Aliasing information is represented with an aliasing graph.
   * @param labels nodes associated to each variable
   * @param edges the edge set of the graph.
   * @param types declared types of variables.
   */
  class Property(private[objects] val labels: Seq[Option[Node]], val edges: EdgeSet, private[objects] val types: Seq[om.Type]) extends ObjectProperty[Property] {

    checkInvariant()

    /**
     * This method checks if an aliasing graph is well formed.
     */
    @elidable(ASSERTION)
    private def checkInvariant() {
      assume(labels.length == types.length, s"Field `labels` has length ${labels.length} while field `types` has length ${types.length} in ${this}")
      for (Some(n) <- labels) { assume(edges.isDefinedAt(n), s"First level node ${n} has no corresponding span") }
      for (n <- edges.keys) { assume(labels contains Some(n), s"A span exists for node ${n} which is not first level in ${this}") }
      for (n <- nodes) { assume(completeNodeType(n).isDefined, s"Node ${n} has no defined type in ${this}") }
      for (Some(n) <- labels.toSet; t = nodeType(n); (f, tgt) <- edges(n); fields = om.fieldsOf(t) ) { 
        assume (fields contains f, s"node ${n} contains an invalid field ${f} in ${this}")
      }  
    }

    /**
     * Determines whether the node `n` is a 1^ level (root) node
     */
    private[objects] def isFirstLevel(n: Node): Boolean = {
      labels contains Some(n)
    }
    
    /**
     * Returns the set of nodes reachable by 1^ level nodes in `nodes`.
     */
    private[objects] def reachableNodesFrom(nodes: Node*): Set[Node] = {
      assume(nodes forall { isFirstLevel(_) } )
      val s = collection.mutable.Set(nodes: _*)
      val q = collection.mutable.Queue(nodes: _*)
      while (!q.isEmpty) {
        val n = q.dequeue
        if (isFirstLevel(n)) {
          for ((f, tgt) <- edges(n); if !(s contains tgt)) {
            s += tgt
            q.enqueue(tgt)
          }
        }
      }
      s.toSet
    }

    /**
     * Returns the set of nodes reachable from the set of of all 1^ level without passing
     * through node `forbidden`.
     * @note This is more efficient that just resorting to reachableNodesFrom
     */
    private[objects] def reachableNodesForbidden(forbidden: Node): Set[Node] = {
      assume(isFirstLevel(forbidden))      
      val firstLevel: Set[Node] = (for (Some(n) <- labels; if n != forbidden) yield n) (collection.breakOut)     
      val secondLevel = (firstLevel flatMap { edges(_).values }) - forbidden
      firstLevel ++ secondLevel
    }

    /**
     * Returns the set of all the nodes in the graph.
     * @note This is more efficient that just resorting to reachableNodesFrom
     */
    private[objects] def nodes: Set[Node] = {
      val firstLevel: Set[Node] = (for (Some(n) <- labels) yield n) (collection.breakOut)     
      val secondLevel = (firstLevel flatMap { edges(_).values })
      firstLevel ++ secondLevel      
    }

    /**
     * Determines whether we may reach a 2^ level node from a given 1^ level node.
     */
    private[objects] def escape(n: Node): Boolean = {
      assume(isFirstLevel(n))
      edges(n).values exists { ! isFirstLevel(_) }
    }

    /**
     * Returns the set of 1^ level nodes which contains the provided field.
     */
    private[objects] def nodesWithField(field: om.Field): Set[Node] = {
      (for {
        (on, nt) <- labels zip types
        n <- on
        if om.fieldsOf(nt) contains field
      } yield n)(collection.breakOut)      
    }

    /**
     * Returns the node associated with a reachable identifier, or `None` otherwise.
     * We assume f is in the type of the identifier v
     */
    private[objects] def nodeOf(v: Int, f: om.Field): Option[Node] = {
      assume ( om.pathExists(types(v), f) )
      labels(v) flatMap { edges(_).get(f) }
    }

    /**
     * Returns the node associated with a field, or `None` otherwise.
     * We assume the input is well typed
     */
    private[objects] def nodeOf(v: Int, fs: Iterable[om.Field]): Option[Node] = {
      assume ( om.pathExists(types(v), fs.toSeq: _*) )
      val on = labels(v)
      if (on.isEmpty) None else nodeOf(on.get, fs)
    }

    /**
     * Follow the chain of fields starting from node `n` and returns the nodes we reach,
     * or `None` if some fields is not defined in the graph.
     */
    @tailrec
    private def nodeOf(n: Node, fs: Iterable[om.Field]): Option[Node] = {
      if (fs.isEmpty)
        Some(n)
      else edges(n).get(fs.head) match {
        case None => None
        case Some(nnew) => nodeOf(nnew, fs.tail)
      }
    }
       
    /**
     * Returns the glb of all the types of variables pointing to 
     * 'n`. We assume `n` is a 1^ level node
     */
    private[objects] def nodeType(n: Node): om.Type = {
      assume(isFirstLevel(n), s"nodeType called with non 1^ level node n${n}")
      om.glb(for ((Some(`n`), t) <- labels zip types) yield t).get
    }

    /**
     * Returns a glb approximation of all types pointing to node n.
     * This could be used in the future to improve precision in the analysis,
     * but at the moment we are consistent with the paper.
     */
    private[objects] def completeNodeType(n: Node): Option[om.Type] = {
      val firstLevels = for ((Some(`n`), t) <- labels zip types) yield t
      val secondLevels = for ((_, span) <- edges; (f, `n`) <- span) yield om.typeOf(f)
      om.glb(firstLevels ++ secondLevels)
    }

    /**
     * Returns the new set of edges of node `n` assuming its type becomes `t`. If `t`
     * is not a subtype of the current type of `n`, it does nothing.
     */
    private[objects] def expandSpan(n: Node, t: om.Type): Span = {
      assume ( isFirstLevel(n) )
      var span = edges(n)
      val nodet = nodeType(n)
      if (om.lteq(t, nodet))
        for (f <- om.fieldsOf(t) -- om.fieldsOf(nodet); if !(span isDefinedAt f)) span += f -> Node()
      span
    }

    /**
     * Returns the edges removed from node `n` when its type becomes `t`. We assume that
     * `t` is a supertype of the type of `n`
     */
    private[objects] def reduceSpan(n: Node, t: om.Type): (Span, Iterable[Node]) = {
      assume ( isFirstLevel(n) &&  om.lteq(nodeType(n), t) )
      val removedFields = om.fieldsOf(nodeType(n)) -- om.fieldsOf(t)
      val removedNodes = for { (f, dst) <- edges(n); if removedFields contains f } yield dst
      (edges(n) -- removedFields, removedNodes)
    }

    /**
     * Returns a new edge-set if variable `v` does not point to `labels(v)` anymore
     */
    private def reducedEdgeSet(v: Int): EdgeSet = {
      labels(v) match {
        case None => edges
        case on @ Some(n) => if (labels.count(_ == on) == 1) edges - n else edges
      }
    }
        
    /**
     * Returns, if it exists, a morphism connecting `this` and `other`.
     * @param other the other graph, which should be on the same fiber of `this`
     * @returns `None` if the two graphs are incomparable, otherwise `Some(i,m)` where `i`
     * is the same result of `tryCompare` and `m` is the morphism, either `this |-> other`
     * or `other |-> this` according to the value of `i`.
     */
    private[objects] def tryMorphism(other: Property): Option[(Int, Morphism)] = {
      assume(fiber == other.fiber)

      class MorphismBuilder {
        private var status: Option[Int] = Some(0)
        private var map1 = Map[Node, Option[Node]]()
        private var map2 = Map[Node, Option[Node]]()

        def direction = this.status

        def morphism: Option[Morphism] = status match {
          case None => None
          case Some(-1) => Some(map2.withDefaultValue(None))
          case _ => Some(map1.withDefaultValue(None))
        }

        def matchFields(n1: Node, n2: Node) = {
          if (isFirstLevel(n1) && other.isFirstLevel(n2)) {
            for ((f, n) <- edges(n1)) matchNode(Some(n), other.edges(n2).get(f))
            for ((f, n) <- other.edges(n2)) matchNode(edges(n1).get(f), Some(n))
          }
        }

        def matchNode(on1: Option[Node], on2: Option[Node]): Option[Int] = {
          (status, on1, on2) match {
            case (Some(0), Some(n1), Some(n2)) =>
              (map1 isDefinedAt n1, map2 isDefinedAt n2) match {
                case (false, false) =>
                  map1 += n1 -> on2
                  map2 += n2 -> on1
                  matchFields(n1, n2)
                case (true, false) =>
                  map2 += n2 -> on1
                  status = Some(-1)
                  matchFields(n1, n2)
                case (false, true) =>
                  map1 += n1 -> on2
                  status = Some(1)
                  matchFields(n1, n2)
                case (true, true) =>
                  status = if (map1(n1) == on2) status else None
              }
            case (Some(-1), Some(n1), Some(n2)) =>
              if (!(map2 isDefinedAt n2)) {
                map2 += n2 -> on1
                matchFields(n1, n2)
              } else
                status = if (map2(n2) != on1) None else status
            case (Some(1), Some(n1), Some(n2)) =>
              if (!(map1 isDefinedAt n1)) {
                map1 += n1 -> on2
                matchFields(n1, n2)
              } else
                status = if (map1(n1) != on2) None else status
            case (Some(1), None, Some(n2)) =>
              status = None
            case (Some(-1), Some(n1), None) =>
              status = None
            case (_, None, Some(n2)) =>
              if (!(map2 isDefinedAt n2)) {
                status = Some(-1)
                map2 += n2 -> None
              } else
                status = if (map2(n2) == None) status else None
            case (_, Some(n1), None) =>
              if (!(map1 isDefinedAt n1)) {
                status = Some(1)
                map1 += n1 -> None
              } else
                status = if (map1(n1) == None) status else None
            case _ =>
          }
          status
        }
      }

      val morphismBuilder = new MorphismBuilder()
      for (i <- 0 until labels.length)
        morphismBuilder.matchNode(labels(i), other.labels(i))
      morphismBuilder.direction match {
        case None => None
        case Some(d) => Some((d, morphismBuilder.morphism.get))
      }
    }

    /**
     * Apply a morphism to a graph.
     */
    private[objects] def applyMorphism(m: Morphism): Property = {
      val newLabels = labels map { _ flatMap m }
      val newEdges = for {
        (src, span) <- edges
        newsrc = m(src)
        if newsrc.isDefined
      } yield newsrc.get -> (for {
        (f, dst) <- span
        newdst = m(dst)
        if newdst.isDefined
      } yield f -> newdst.get)
      new Property(newLabels, newEdges, types)
    }

    type Domain = AliasingDomain.this.type

    def domain = AliasingDomain.this

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = {
      other match {
        case other: Property =>
          assume(fiber == other.fiber)
          tryMorphism(other) map { _._1 }
        case _ => None
      }
    }

    def widening(that: Property): Property = this union that

    def narrowing(that: Property): Property = that

    private[objects] def unionWithMorphisms(other: Property) = {

      /**
       * This class build the union of two aliasing graph and, at the same time, the two morphisms
       * to the union. It works by visiting the two graphs in lock-step (while it is possible) and
       * duplicating subgraphs when it is needed.
       */
      class UnionBuilder {
        val map1 = collection.mutable.Map[Node, Node]()
        val map2 = collection.mutable.Map[Node, Node]()
        val newedges = collection.mutable.Map[Node, Span]().withDefaultValue(Map.empty)
        private val reachable1 = collection.mutable.Set[Node]()
        private val reachable2 = collection.mutable.Set[Node]()

        def copySubgraph(g: Property, reachable: collection.mutable.Set[Node], map: collection.mutable.Map[Node, Node], node: Node): Node = {
          map.get(node) match {
            case None =>
              val newnode = Node()
              reachable += newnode
              map(node) = newnode
              if (g.edges.isDefinedAt(node))
                newedges(newnode) = for ((f, n) <- g.edges(node)) yield (f, copySubgraph(g, reachable, map, n))
              newnode
            case Some(newnode) =>
              newnode
          }
        }

        def matchFields(newnode: Node, n1: Node, n2: Node): Node = {
          reachable1 += newnode
          reachable2 += newnode
          if (isFirstLevel(n1) && other.isFirstLevel(n2)) {
            var newspan: Span = Map.empty
            for ((f, n) <- edges(n1)) {
              val newtgt = matchNode(Some(n), other.edges(n2).get(f))
              newspan += f -> newtgt
            }
            for ((f, n) <- other.edges(n2)) {
              val newtgt = matchNode(edges(n1).get(f), Some(n))
              newspan += f -> newtgt
            }
            newedges(newnode) = newspan
          }
          newnode
        }

        def matchNode(on1: Option[Node], on2: Option[Node]): Node = {
          (on1, on2) match {
            case (Some(n1), Some(n2)) =>
              (map1 isDefinedAt n1, map2 isDefinedAt n2) match {
                case (false, false) =>
                  map1 += n1 -> n1
                  map2 += n2 -> n1
                  matchFields(n1, n1, n2)
                case (true, false) =>
                  val newnode = Node()
                  map2 += n2 -> newnode
                  matchFields(newnode, n1, n2)
                case (false, true) =>
                  val newnode = Node()
                  map1 += n1 -> newnode
                  matchFields(newnode, n1, n2)
                case (true, true) =>
                  if (map1(n1) == map2(n2))
                    map1(n1)
                  else
                    matchFields(Node(), n1, n2)
              }
            case (None, Some(n2)) =>
              map2.get(n2) match {
                case None =>
                  if (other.isFirstLevel(n2)) newedges(n2) = other.edges(n2)
                  map2(n2) = n2
                  n2
                case Some(n) =>
                  if (reachable1 contains n) {
                    copySubgraph(other, reachable1, collection.mutable.Map(), n2)
                  } else
                    n
              }
            case (Some(n1), None) =>
              map1.get(n1) match {
                case None =>
                  if (isFirstLevel(n1)) newedges(n1) = edges(n1)
                  map1(n1) = n1
                  n1
                case Some(n) =>
                  if (reachable2 contains n) {
                    copySubgraph(Property.this, reachable2, collection.mutable.Map(), n1)
                  } else
                    n
              }
            case _ =>
              throw new IllegalStateException("This should never happen")
          }
        }
      }

      val unionBuilder = new UnionBuilder()
      val newlabels = for (i <- 0 until labels.length) yield {
        val n1 = labels(i)
        val n2 = other.labels(i)
        if (n1.isDefined || n2.isDefined)
          Some(unionBuilder.matchNode(labels(i), other.labels(i)))
        else
          None
      }
      val newedges = unionBuilder.newedges.toMap.filterKeys { newlabels contains Some(_) }
      val unionGraph = new Property(newlabels, newedges , types)
      (unionGraph, unionBuilder.map1, unionBuilder.map2)
    }

    def union(other: Property): Property = (this unionWithMorphisms other)._1

    private[objects] def intersectionWithMorphisms(other: Property): (Property, Morphism, Morphism) = {
      val partition = DisjointSets[(Boolean, Option[Node])]((false, None), (true, None))
      // the boolean value is used to differentiate between nodes coming from the two graphs

      def computePartition(on1: Option[Node], on2: Option[Node]): Unit = {
        if (!partition.inSamePartition((false, on1), (true, on2))) {
          partition.union((false, on1), (true, on2))
          (on1, on2) match {
            case (Some(n1), Some(n2)) if isFirstLevel(n1) && other.isFirstLevel(n2) =>
              for ((f, n) <- edges(n1)) computePartition(Some(n), other.edges(n2).get(f))
              for ((f, n) <- other.edges(n2)) computePartition(edges(n1).get(f), Some(n))
            case (Some(n1), None) if isFirstLevel(n1)  =>
              for ((f, n) <- edges(n1)) computePartition(Some(n), None)
            case (None, Some(n2)) if other.isFirstLevel(n2) =>
              for ((f, n) <- other.edges(n2)) computePartition(None, Some(n))
            case (None, None) =>
              throw new IllegalStateException("We should never reach this state")
            case (_, _) =>
          }
        }
      }

      def mapWithPartition(n: (Boolean, Option[Node]), nullNode: (Boolean, Option[Node])): Option[Node] = {
        val repr = partition(n)
        if (repr == nullNode) None else repr._2
      }

      def partitionToMap(nullNode: (Boolean, Option[Node])): Morphism = { (n: Node) =>
        val repr = partition((false, Some(n)))
        if (repr == nullNode) None else repr._2
      }

      partition.union((false, None), (true, None))
      for ((on1, on2) <- labels zip other.labels)
        computePartition(on1, on2)

      val nullNode = partition((false, None))
      val newLabels = labels map { n => mapWithPartition((false, n), nullNode) }
      val newEdges = for {
        (src, span) <- edges; reprsrc <- partition.find((false, Some(src))); if reprsrc != nullNode
      } yield {
        reprsrc._2.get -> (for { (f, tgt) <- span; reprtgt = partition((false, Some(tgt))); if reprtgt != nullNode }
          yield f -> reprtgt._2.get)
      }
      val newGraph = new Property(newLabels, newEdges, types)
      val morphism = partitionToMap(nullNode)
      (newGraph, morphism, morphism)
    }

    def intersection(other: Property): Property = (this intersectionWithMorphisms other)._1

    def isEmpty: Boolean = false

    def fiber = types

    def dimension = types.length

    def addUnknownVariable(t: om.Type): Property = {
      new Property(labels :+ None, edges, types :+ t)
    }

    /**
     * Returns the aliasing graph obtained by adding a fresh variable, together
     * with the node corresponding to its variable and its fresh span.
     */
    private[objects] def addFreshVariableWithSpan(t: om.Type): (Property, Node, Span) = {
      val n = Node()
      val span: Span = om.fieldsOf(t).map { _ -> Node() }(collection.breakOut)
      val newgraph = new Property(labels :+ Some(n), edges updated (n, span), types :+ t)
      (newgraph, n, span)
    }

    def addFreshVariable(t: om.Type): Property = addFreshVariableWithSpan(t)._1

    def addVariable(t: om.Type): Property = {
      addFreshVariable(t)
    }

    def delVariable(v: Int): Property = {
      // Remove spurious edges
      val newedges = reducedEdgeSet(v)
      if (v == dimension - 1)
        new Property(labels.dropRight(1), newedges, types.dropRight(1))
      else
        new Property(labels.take(v) ++ labels.drop(v + 1), newedges, types.take(v) ++ types.drop(v + 1))
    }

    def mapVariables(rho: Seq[Int]): Property = {
      val newsize = rho.count(_ != -1)
      val revrho = collection.mutable.Buffer.fill(newsize)(0)
      for ((newidx, oldidx) <- rho.zipWithIndex; if newidx != -1) revrho(newidx) = oldidx
      mapVariablesReverse(revrho)
    }

    /**
     * It works like mapVariables, but if `rho(i)=j` then variable `j` in the
     * original aliasing graph becomes variable `i`.
     */
    private def mapVariablesReverse(rho: Seq[Int]): Property = {
      val newlabels = rho map labels
      val newtypes = rho map types
      val newedges = edges filterKeys { newlabels contains Some(_) }
      new Property(newlabels, newedges, newtypes)
    }

    def top: Property = domain.top(fiber)

    def bottom: Property = domain.bottom(fiber)

    def isTop: Boolean = this == top

    def isBottom: Boolean = labels forall { _.isEmpty }

    def connect(other: Property, common: Int): Property = {
      // we check wether it is possible to reach the non-common variable in this from the common
      // variables. Here sharing might help.
      val escapeFromCommon = (labels zip types) exists { case (Some(n), t) => escape(n); case _ => false }
      val newlabels = labels.dropRight(common) ++ other.labels.drop(common)
      val newtypes = types.dropRight(common) ++ other.types.drop(common)
      
      if (!escapeFromCommon) {
        val newedges = (edges ++ other.edges).filterKeys { newlabels contains Some(_) }
        new Property(newlabels, newedges, newtypes)          
      } else {
        // set of 1st level nodes reachable in the non-common part of this
        val reachableNodes: Set[Node] = (for (on <- labels; n <- on) yield n)(collection.breakOut)
        val newedges =
          for ((src, span) <- edges) yield src ->
            (if (reachableNodes contains src)
              fullSpan(nodeType(src))
            else
              span)
        new Property(
          newlabels,
          (newedges ++ other.edges).filterKeys { newlabels contains Some(_) },
          newtypes)
      }
    }

    def mkString(vars: Seq[String]) = {
      val s1 = for { (Some(n), v) <- labels.zipWithIndex } yield {
        s"${vars(v)} : ${types(v)} -> n${n}"
      }
      val s2 = for { Some(n) <- labels.toSet; (f, ntgt) <- edges(n) } yield {
        // toSet is needed to remove duplicates
        s"n${n} -- ${f} --> n${ntgt}"
      }
      s1.mkString("Vars: ", ", ", "") + " // " + s2.mkString("Edges: ", ", ", "")
    }

    def typeOf(v: Int, fs: Iterable[om.Field]) = nodeOf(v, fs) map nodeType

    def assignNull(dst: Int = dimension - 1): Property = {
      new Property(labels.updated(dst, None), reducedEdgeSet(dst), types)
    }

    def assignVariable(dst: Int, src: Int): Property = {
      if (dst == src)
        this
      else labels(src) match {
        case None =>
          assignNull(dst)
        case Some(src) =>
          new Property(labels.updated(dst, Some(src)), reducedEdgeSet(dst), types)
      }
    }

    def assignVariableToField(dst: Int, field: om.Field, src: Int): Property = {
      (labels(dst), labels(src)) match {
        case (None, _) =>
          bottom
        case (Some(dstNode), None) =>
          val newspan = edges(dstNode) - field
          new Property(labels, edges.updated(dstNode, newspan), types)
        case (Some(dstNode), Some(srcNode)) =>
          val newspan = edges(dstNode) + (field -> srcNode)
          // we need to fresh all node which may possibly be reached by dst.field. In the presence of
          // sharing, we may restrict possibleAliases using sharing information
          val possibleAliases = nodesWithField(field)
          val newedges =
            for ((n, span) <- edges.updated(dstNode, newspan)) yield if (n == dstNode)
              n -> span
            else if (isFirstLevel(n) && (possibleAliases contains n))
              n -> span.updated(field, Node())
            else
              n -> span
          new Property(labels, newedges, types)
      }
    }

    def assignFieldToVariable(dst: Int, src: Int, field: om.Field): Property = {
      if (mustBeNull(src))
        bottom
      else {
        nodeOf(src, field) match {
          case None => assignNull(dst)
          case on @ Some(n) =>
            val newedges =
              if (on == labels(dst))
                edges
              else if (isFirstLevel(n))
                reducedEdgeSet(dst)
              else reducedEdgeSet(dst).updated(n, fullSpan(types(dst)))
            new Property(labels.updated(dst, on), newedges, types)
        }
      }
    }

    /**
     *  Return the result of a cast and the new span added to the variable v and its node
     */
    private[objects] def castVariableWithSpan(v: Int, newtype: om.Type): (Property, Option[Node], Span) = {
      assume(om.lteq(newtype, types(v)))
      labels(v) match {
        case None =>
          val newgraph = new Property(labels, edges, types updated (v, newtype))
          (newgraph, None, Map.empty)
        case Some(n) =>
          val newspan = expandSpan(n, newtype)
          val newgraph = new Property(labels, edges updated (n, newspan), types updated (v, newtype))
          (newgraph, Some(n), newspan)
      }
    }

    def castVariable(v: Int, newtype: om.Type): Property = castVariableWithSpan(v, newtype)._1

    /**
     * Returns the result of a nullness test and the nodes reachable from the tested variable.
     */
    private[objects] def testNullWithNodes(v: Int): (Property, collection.Set[Node]) = {
      labels(v) match {
        case None => (this, Set.empty)
        case Some(nullnode) =>
          val nodes = reachableNodesFrom(nullnode)
          val newlabels = labels map { _ flatMap { n => if (nodes contains n) None else Some(n) } }
          val newedges = (edges -- nodes) mapValues { span => span filterNot { case (f, n) => nodes contains n } }
          val newgraph = new Property(newlabels, newedges, types)
          (newgraph, nodes)
      }
    }

    def testNull(v: Int): Property = testNullWithNodes(v)._1

    def testNotNull(v: Int): Property = {
      if (mustBeNull(v))
        bottom
      else
        this
    }

    def mustBeNull(v: Int, fs: Iterable[om.Field]) = nodeOf(v, fs).isEmpty

    def mayBeNull(v: Int, fs: Iterable[om.Field]) = true

    def mayShare(v1: Int, fs1: Iterable[om.Field], v2: Int, fs2: Iterable[om.Field]) = (nodeOf(v1, fs1), nodeOf(v2, fs2)) match {
      case (Some(n1), Some(n2)) => true
      case _ => false
    }

    def mustShare(v1: Int, fs1: Iterable[om.Field], v2: Int, fs2: Iterable[om.Field]) = false

    def mayBeAliases(v1: Int, v2: Int) = labels(v1).isDefined && labels(v2).isDefined && om.mayBeAliases(types(v1), types(v2))

    def mustBeAliases(v1: Int, v2: Int) = labels(v1).isDefined && labels(v2) == labels(v1)

    def mayBeWeakAliases(v1: Int, v2: Int) = true

    def mustBeWeakAliases(v1: Int, v2: Int) = labels(v1) == labels(v2)
  }
}

object AliasingDomain extends ObjectDomainFactory {

  /**
   * A node in an aliasing graph. The current implementation use integers to represent
   * nodes, although boxing will probably eat all the performance benefits we could
   * gain from this choice. Nodes are significant only within the same graph.
   */
  class Node private (val i: Integer) extends AnyVal {
    override def toString() = i.toString
  }

  /**
   * The companion object for nodes. It only has an apply method which creates new fresh node.
   */
  object Node {
    /**
     * An internal counter used to create fresh nodes
     */
    private var current: Int = 0

    /**
     * Returns a fresh node, i.e., a node which is guaranteed to be different from all the
     * other nodes.
     */
    def apply(): Node = {
      current += 1
      new Node(current)
    }
  }

  /**
   * A morphism is a function from Node to Option[Node]. We could use
   * partial function, but I think a standard function is more convenient.
   */
  type Morphism = Function[Node, Option[Node]]

  def apply[OM <: ObjectModel](om: OM) = new AliasingDomain(om)
}
