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

import scala.collection.breakOut

/**
 * The domain for the ALPs domain.0
 * @author Gianluca Amato <gamato@unich.it>
 */
class ALPsDomain[OM <: ObjectModel](val om: OM) extends ObjectDomain[OM] {

  def top(types: Seq[om.Type]) = {
    val labels = for { t <- types } yield if (om.mayShare(t, t)) Some(new Node) else None
    val edges = (for { (t, Some(n)) <- types zip labels } yield {
      val outspan = for { f <- om.fieldsOf(t); tf = om.typeOf(f); if om.mayShare(tf, tf) } yield f -> new Node()
      n -> outspan.toMap
    })
    new Property(labels, edges.toMap.withDefaultValue(Map.empty), types)
  }

  def bottom(types: Seq[om.Type]) = new Property(Seq.fill(types.size)(None), Map().withDefaultValue(Map.empty), types)

  def apply(labels: Seq[Option[Node]], edges: Map[Node, Map[om.Field, Node]], types: Seq[om.Type]): Property =
    new Property(labels, edges.withDefaultValue(Map.empty), types)

  def apply(labels: Seq[Option[Node]], edges: Seq[(Node, om.Field, Node)], types: Seq[om.Type]): Property = {
    val graphEdges =
      edges.groupBy(_._1).
        mapValues(_.groupBy(_._2).
          mapValues(_.head._3))
    apply(labels, graphEdges, types)
  }

  /**
   * This is a node in the ALPs graph.
   * @param nodeType is the minimum class of the variables bound to this node. None means that
   * there are no variable bound to this node. It could be computed by other informations in a graph,
   * but it should be faster to keep it locally.
   * @param edges is the map from fields to nodes. It is a variable only because it is needed for self loops
   */
  /*class Node(val nodeType: Option[om.Type] = None, var edges: Map[om.Field, Node] = Map()) {
    /**
     * Expand a node adding all fields in om.Type
     */
    def expand(t: om.Type) = {
      var newedges = edges
      if (nodeType.isEmpty || om.lt(t, nodeType.get)) {
        for (f <- om.fieldsOf(t); if !(edges isDefinedAt f)) newedges += f -> new Node()
        new Node(Some(t), newedges)
      } else
        this
    }
    override def toString = s"n${this.hashCode()}"
  }*/

  class Node extends AnyRef {
    override def toString = s"n${hashCode().toString}"
  }

  type EdgeSet = Map[Node, Map[om.Field, Node]]

  type ALPsMorphism = Map[Node, Option[Node]]

  case class Property(labels: Seq[Option[Node]], edges: EdgeSet, types: Seq[om.Type]) extends ObjectProperty[Property] {

    type Domain = ALPsDomain.this.type

    def domain = ALPsDomain.this

    /**
     * Returns the type of a node `n`, the least type of all the variables bound to `n`
     */
    private def nodeType(n: Node): Option[om.Type] = {
      var nodet: Option[om.Type] = None
      for ((Some(`n`), t) <- labels zip types) {
        nodet = if (nodet == None) Some(t) else Some(om.min(nodet.get, t))
      }
      nodet
    }

    /**
     * Returns the new set of edges of node `n` assuming its type becomes t
     */
    private def expandSpan(n: Node, t: om.Type) = {
      var span = edges(n)
      val nodet = nodeType(n)
      if (nodet.isEmpty || om.lt(t, nodet.get))
        for (f <- om.fieldsOf(t) -- om.fieldsOf(nodet.get); if !(span isDefinedAt f)) span += f -> new Node
      span
    }

    /**
     *  Returns the set of nodes reachable by `n`
     */
    private def reachableNodes(n: Node) = {
      val s = collection.mutable.Set(n)
      val q = collection.mutable.Queue(n)
      while (!q.isEmpty) {
        val n = q.dequeue
        for ((f, tgt) <- edges(n); if !(s contains tgt)) {
          s += tgt
          q.enqueue(tgt)
        }
      }
      s
    }

    /**
     * Returns the node of the variable i of the graph, None if the variable is null
     */
    def labelOf(i: Int) = labels(i)

    /**
     * Returns the node of the qualified field i.f, None if is null
     */
    def labelOf(i: Int, field: om.Field) = {
      labels(i) flatMap { edges(_).get(field) }
    }

    /**
     * Returns the type of the variable i
     */
    def typeOf(i: Int) = types(i)

    def widening(that: Property): Property = this.union(that)

    def narrowing(that: Property): Property = this.intersection(that)

    def union(other: Property): Property = {

      class UnionBuilder {
        private val map1 = collection.mutable.Map[Node, Node]()
        private val map2 = collection.mutable.Map[Node, Node]()
        private val reachable1 = collection.mutable.Set[Node]()
        private val reachable2 = collection.mutable.Set[Node]()
        private val newedges = collection.mutable.Map[Node, Map[om.Field, Node]]().withDefaultValue(Map())

        def getEdges = newedges.toMap.withDefaultValue(Map())

        def copySubgraph(reachable: collection.mutable.Set[Node], node: Node): Node = {
          val newnode = new Node
          reachable += newnode
          val span = for ((f, n) <- newedges(node)) yield (f, copySubgraph(reachable,n))
          newedges(newnode) = span
          newnode
        }

        def matchFields(newnode: Node, n1: Node, n2: Node) = {
          reachable1 += newnode
          reachable2 += newnode
          for ((f, n) <- edges(n1)) {
            val newtgt = matchNode(Some(n), other.edges(n2).get(f))
            newedges(newnode) += f -> newtgt
          }
          for ((f, n) <- other.edges(n2)) {
            val newtgt = matchNode(edges(n1).get(f), Some(n))
            newedges(newnode) += f -> newtgt
          }
        }

        def matchNode(on1: Option[Node], on2: Option[Node]): Node = {
          //println(on1, on2)
          (on1, on2) match {
            case (Some(n1), Some(n2)) =>
              (map1 isDefinedAt n1, map2 isDefinedAt n2) match {
                case (false, false) =>
                  map1 += n1 -> n1
                  map2 += n2 -> n1
                  matchFields(n1, n1, n2)
                  n1
                case (true, false) =>
                  val newnode = new Node
                  map1 += n1 -> newnode
                  matchFields(newnode, n1, n2)
                  newnode
                case (false, true) =>
                  val newnode = new Node
                  map2 += n2 -> newnode
                  matchFields(newnode, n1, n2)
                  newnode
                case (true, true) =>
                  if (map1(n1) == map2(n2))
                    map1(n1)
                  else {
                    val newnode = new Node
                    matchFields(newnode, n1, n2)
                    newnode
                  }
              }
            case (None, Some(n2)) =>
              map2.get(n2) match {
                case None =>
                  newedges(n2) = other.edges(n2)
                  map2(n2) = n2
                  n2
                case Some(n) =>
                  if (reachable1 contains n)
                    copySubgraph(reachable1, n)
                  else
                    n
              }
            case (Some(n1), None) =>
              map1.get(n1) match {
                case None =>
                  newedges(n1) = edges(n1)
                  map1(n1) = n1
                  n1
                case Some(n) =>
                  if (reachable2 contains n)
                    copySubgraph(reachable2, n)
                  else
                    n
              }
            case _ =>
              throw new IllegalStateException("This should never happen")
          }
        }
      }

      val unionBuilder = new UnionBuilder()
      val newlabels = for (i <- 0 until labels.length) yield {
        val n1 = labelOf(i)
        val n2 = other.labelOf(i)
        if (n1.isDefined || n2.isDefined)
          Some(unionBuilder.matchNode(labelOf(i), other.labelOf(i)))
        else
          None
      }
      new Property(newlabels, unionBuilder.getEdges, types)
    }

    def intersection(that: Property): Property = ???

    def isEmpty: Boolean = false

    def mapVariables(rho: Seq[Int]): Property = {
      val newsize = rho.count(_ != -1)
      val revrho = collection.mutable.Buffer.fill(newsize)(0)
      for ((newidx, oldidx) <- rho.zipWithIndex; if newidx != -1) revrho(newidx) = oldidx
      mapVariablesReverse(revrho)
    }

    def mapVariablesReverse(rho: Seq[Int]): Property = {
      new Property(rho map labels, edges, rho map types)
    }

    def top: Property = domain.top(fiber)

    def bottom: Property = domain.bottom(fiber)

    def isTop: Boolean = this == top

    def isBottom: Boolean = labels forall { _.isEmpty }

    def connect(other: Property, common: Int): Property = ???

    def fiber = types

    def dimension = labels.size

    def addFreshVariable(t: om.Type): Property = {
      val n = new Node
      val span: Map[om.Field, Node] = om.fieldsOf(t).map { _ -> new Node }(collection.breakOut)
      new Property(labels :+ Some(n), edges updated (n, span), types :+ t)
    }

    def addVariable(t: om.Type): Property = {
      addFreshVariable(t)
    }

    def delVariable(v: Int): Property = {
      if (v == dimension - 1)
        new Property(labels.dropRight(1), edges, types.dropRight(1))
      else
        new Property(labels.take(v) ++ labels.drop(v + 1), edges, types.take(v) ++ types.drop(v + 1))
    }

    def assignNull(dst: Int = dimension - 1): Property = {
      Property(labels.updated(dst, None), edges, types)
    }

    def assignVariable(dst: Int, src: Int): Property = {
      labelOf(src) match {
        case None =>
          assignNull(dst)
        case Some(src) =>
          Property(labels.updated(dst, Some(src)), edges, types)
      }
    }

    def assignVariableToField(dst: Int, field: om.Field, src: Int): Property = {
      labelOf(dst) match {
        case None =>
          bottom
        case Some(dst) =>
          val nodesrc = labelOf(src)
          val newspan = if (nodesrc == None) edges(dst) - field else edges(dst) + (field -> nodesrc.get)
          Property(labels, edges.updated(dst, newspan), types)
      }
    }

    def assignFieldToVariable(dst: Int, src: Int, field: om.Field): Property = {
      if (isDefiniteNull(src))
        bottom
      else {
        labelOf(src, field) match {
          case None => assignNull(dst)
          case Some(src) =>
            Property(labels.updated(dst, Some(src)), edges, types)
        }
      }
    }

    def castVariable(v: Int, newtype: om.Type): Property = {
      labelOf(v) match {
        case None => Property(labels, edges, types updated (v, newtype))
        case Some(n) => Property(labels, edges updated (n, expandSpan(n, newtype)), types updated (v, newtype))
      }
    }

    def isDefiniteNull(v: Int, fieldseq: Seq[om.Field]): Boolean = {
      val loc = labelOf(v)
      (loc, fieldseq) match {
        case (None, _) => true
        case (Some(node), Seq(f)) => edges(node).isDefinedAt(f)
        case _ => false
      }
    }

    def testNull(v: Int): Property = {
      labelOf(v) match {
        case None => this
        case Some(nullnode) =>
          val nodes = reachableNodes(nullnode)
          val newlabels = labels map { _ flatMap { n => if (nodes contains n) None else Some(n) } }
          val newedges = edges mapValues { span => span filterNot { case (f, n) => nodes contains n } }
          /**
           * @todo We need to repeat here the withDefaultValue option.... we probably should consider rewriting
           * stuff so that we do no need them
           */
          new Property(newlabels, newedges withDefaultValue (Map()), types)
      }
    }

    def testNotNull(v: Int): Property = {
      if (isDefiniteNull(v))
        bottom
      else
        this
    }

    /**
     * Returns, if it exists, a morphism connecting `this` and `other`.
     * @param other the other graph. We assume the two graphs are over the same fiber, since
     * we do not check this property.
     * @returns `None` if the two graphs are incomparable, otherwise `Some(i,m)` where `i`
     * is the same result of `tryCompare` and `m` is the morphism, either `this |-> other`
     * or `other |-> this` according to the value of `i`.
     */
    def tryMorphism(other: Property): Option[(Int, ALPsMorphism)] = {

      class MorphismBuilder {
        private var status: Option[Int] = Some(0)
        private var map1 = Map[Node, Option[Node]]()
        private var map2 = Map[Node, Option[Node]]()

        def direction = this.status

        def morphism: Option[ALPsMorphism] = status match {
          case None => None
          case Some(-1) => Some(map2.withDefaultValue(None))
          case _ => Some(map1.withDefaultValue(None))
        }

        def matchFields(n1: Node, n2: Node) = {
          for ((f, n) <- edges(n1)) matchNode(Some(n), other.edges(n2).get(f))
          for ((f, n) <- other.edges(n2)) matchNode(edges(n1).get(f), Some(n))
        }

        def matchNode(on1: Option[Node], on2: Option[Node]): Option[Int] = {
          //println(status, on1, on2)
          (status, on1, on2) match {
            case (Some(0), Some(n1), Some(n2)) =>
              (map1 isDefinedAt n1, map2 isDefinedAt n2) match {
                case (false, false) =>
                  map1 += n1 -> on2
                  map2 += n2 -> on1
                  matchFields(n1, n2)
                case (true, false) =>
                  map1 += n1 -> None
                  status = Some(1)
                  matchFields(n1, n2)
                case (false, true) =>
                  map2 += n2 -> None
                  status = Some(-1)
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
        morphismBuilder.matchNode(labelOf(i), other.labelOf(i))
      morphismBuilder.direction match {
        case None => None
        case Some(d) => Some((d, morphismBuilder.morphism.get))
      }
    }

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = {
      other match {
        case other: Property =>
          // this check might be removed to improve performance
          if (fiber != other.fiber)
            None
          else
            tryMorphism(other) map { _._1 }
        case _ => None
      }
    }

    def mkString(vars: Seq[String]) = mkString(vars, false)

    def mkString(vars: Seq[String], hashValued: Boolean) = {
      val nodenames = collection.mutable.Map[Node, Int]()
      var currnum = -1
      val s1 = for { (Some(n), v) <- labels.zipWithIndex } yield {
        val l = if (hashValued) n.hashCode() else nodenames.getOrElseUpdate(n, { currnum += 1; currnum })
        s"${vars(v)} : ${typeOf(v)} -> n${l}"
      }
      val s2 = for { Some(n) <- labels.toSet; (f, ntgt) <- edges(n) } yield {
        val l1 = if (hashValued) n.hashCode() else nodenames(n)
        val l2 = if (hashValued) ntgt.hashCode() else nodenames.getOrElseUpdate(ntgt, { currnum += 1; currnum })
        s"n${l1} -- ${f} --> n${l2}"
      }
      s1.mkString("Vars: ", ", ", "") + " // " + s2.mkString("Edges: ", ", ", "")
    }
  }
}

object ALPsDomain extends ObjectDomainFactory {
  def apply[OM <: ObjectModel](om: OM) = new ALPsDomain(om)
}
