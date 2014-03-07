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

package it.unich.jandom.domains

import org.scalatest.FunSpec
import org.scalatest.FunSuite
import it.unich.jandom.domains.objects.ALPsDomain
import it.unich.jandom.domains.objects.ObjectModel
import org.scalatest.FlatSpec

/**
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class ALPsSpec extends FunSpec {
  import scala.language.implicitConversions

  def ALPsMorphism[OM <: ObjectModel](om: OM)(dom: ALPsDomain[om.type])(g1: dom.Property, g2: dom.Property, m: dom.ALPsMorphism) {
    it("preserve labels") {
      for (i <- 0 until g1.dimension) {
        assert((g1.labelOf(i) flatMap m) === g2.labelOf(i))
        for (f <- om.fieldsOf(g1.typeOf(i)))
          assert((g1.labelOf(i, f) flatMap m) === g2.labelOf(i, f))
      }
    }
  }

  def nonExtremalGraph[OM <: ObjectModel](om: OM)(dom: ALPsDomain[om.type])(g: dom.Property) {
    val top = g.top
    val bottom = g.bottom

    it("is not bottom") { assert(!g.isBottom) }
    it("is not top") { assert(!g.isTop) }
    it("is not empty") { assert(!g.isEmpty) }
    it("is bigger than bottom") { assert( g > bottom ) }
    it("is smaller than top") { assert ( g < top ) }
    describe("has a morphism to bottom which") {
      val Some((1, m)) = g.tryMorphism(bottom)
      it should behave like ALPsMorphism(om)(dom)(g, bottom, m)
    }
    describe("has a morphism from top which") {
      val Some((-1, m)) = g.tryMorphism(top)
      it should behave like ALPsMorphism(om)(dom)(top, g, m)
    }
  }

  describe("With the trivial object model") {

    object TrivialObjectModel extends ObjectModel {
      type Type = AnyRef
      type Field = Char

      val tsuper = "tsuper"
      val tsub = "tsub"
      val tother = "tother"

      def mayShare(src: Type, tgt: Type) = true
      def fieldsOf(t: Type) = t match {
        case `tsuper` => Seq('a', 'b')
        case `tsub` => Seq('a', 'b', 'c')
        case _ => Seq()
      }
      def typeOf(f: Field) = f match {
        case 'a' => tsuper
        case 'b' => tsuper
        case 'c' => tsub
        case _ => tother
      }
      def lt(t1: Type, t2: Type) = t1 == this.tsub && t2 == this.tsuper
    }

    val dom = ALPsDomain(TrivialObjectModel)
    val om = TrivialObjectModel

    implicit def sizeToTypes(size: Int) = Seq.fill(size)(om.tsuper)

    val g1 = {
      val n0 = new dom.Node()
      val n1 = new dom.Node(Some(om.tsuper),Map('b' -> n0))
      val n2 = new dom.Node(Some(om.tsuper),Map('a' -> n0))
      dom(4, Seq(Some(n2), Some(n1), Some(n1), None))
    }
    val g1a = {
      val n0 = new dom.Node()
      val n1 = new dom.Node(Some(om.tsuper),Map('b' -> n0))
      val n2 = new dom.Node(Some(om.tsuper),Map('a' -> n0))
      dom(4, Seq(Some(n2), Some(n1), None, None))
    }
    val g1b = {
      val n0 = new dom.Node(Some(om.tsuper),Map('a' -> new dom.Node(), 'b' -> new dom.Node()))
      val n1 = new dom.Node(Some(om.tsuper),Map('b' -> n0))
      val n2 = new dom.Node(Some(om.tsuper),Map('a' -> n0))
      dom(4, Seq(Some(n2), Some(n1), Some(n1), Some(n0)))
    }
    val g2 = {
      val n0 = new dom.Node()
      val n1 = new dom.Node(Some(om.tsuper))
      val n2 = new dom.Node(Some(om.tsuper	),Map('a' -> n0))
      dom(4, Seq(Some(n2), Some(n1), Some(n1), None))
    }
    val g3 = {
      val n0 = new dom.Node()
      val n1 = new dom.Node(Some(om.tsuper))
      val n3 = new dom.Node(Some(om.tsuper))
      val n2 = new dom.Node(Some(om.tsuper),Map('a' -> n0, 'b' -> n3))
      dom(4, Seq(Some(n2), Some(n1), Some(n1), Some(n3)))
    }
    val bot4 = dom.bottom(4)
    val top4 = dom.top(4)

    describe("The bottom ALPs graph of dimension 4") {
      val size = 4
      val bot = dom.bottom(size)
      it("has all reachable identifiers labeled by null") {
        for (i <- 0 until size) {
          assert(bot.labelOf(i).isEmpty)
          for (f <- om.fieldsOf(om.tsuper)) {
            assert(bot.labelOf(i, f).isEmpty)
          }
        }
      }
      it("has all identifiers definitively null") {
        for (i <- 0 until size) {
          assert(bot.isDefiniteNull(i))
          for (j <- om.fieldsOf(om.tsuper)) {
            assert(bot.isDefiniteNull(i), Seq(j))
            for (k <- om.fieldsOf(om.typeOf(j))) assert(bot.isDefiniteNull(i), Seq(j, k))
          }
        }
      }
      it("is smaller than top") { bot < dom.top(size) }
      it("is bottom") { assert(bot.isBottom) }
      it("is not top") { assert(!bot.isTop) }
      it("is not empty") { assert(!bot.isEmpty) }
      it("has dimension 4") { assert(bot.dimension === size) }
    }

    describe("The top ALPs graph of dimension 4") {
      val size = 4
      val top = dom.top(size)
      it("has no variable definitively null") {
        for (i <- 0 until size) {
          assert(!top.isDefiniteNull(i))
          for (j <- om.fieldsOf(om.tsuper)) {
            assert(!top.isDefiniteNull(i), Seq(j))
            for (k <- om.fieldsOf(om.typeOf(j))) assert(!top.isDefiniteNull(i), Seq(j, k))
          }
        }
      }
      it("has all reachable identifiers mapped to a node") {
        for (i <- 0 until size) {
          assert(top.labelOf(i).nonEmpty)
          for (f <- om.fieldsOf(om.tsuper)) {
            assert(top.labelOf(i, f).nonEmpty)
          }
        }
      }
      it("is bigger than bottom") { top > dom.bottom(size) }
      it("is not bottom") { assert(!top.isBottom) }
      it("is top") { assert(top.isTop) }
      it("is not empty") { assert(!top.isEmpty) }
      it("has dimension 4") { assert(top.dimension === size) }
    }

    describe("The graph g1") {
      it should behave like nonExtremalGraph(om)(dom)(g1)
      it("has dimension 4") { assert(g1.dimension === 4) }
      it("is not comparable with g2") { assert(g1.tryCompareTo(g2).isEmpty) }
      it("is not comparable with g3") { assert(g1.tryCompareTo(g3).isEmpty) }
    }

    describe("The graph g2") {
      it should behave like nonExtremalGraph(om)(dom)(g2)
      it("has dimension 4") { assert(g2.dimension === 4) }
      it("is not comparable with g1") { assert(g2.tryCompareTo(g1).isEmpty) }
      it("is smaller than g3") { assert(g2 < g3) }
      describe("has a morphism from g3 which") {
        val Some((-1, m)) = g2.tryMorphism(g3)
        it should behave like ALPsMorphism(om)(dom)(g3, g2, m)
      }
    }

    describe("The assignNull method") {
      val g = top4.assignNull(2)
      it("makes variable definitively null") {
        assert(g.isDefiniteNull(2))
      }
      it("should produce a lesser gran than original") {
        assert(g < top4)
      }
      it("maps g1.assignNull(2) to g1a") {
        assert(g1.assignNull(2) === g1a)
      }
    }

    describe("The assignVariable method") {
      it("maps g1.assignVariable(1,2) to g1") {
        assert(g1.assignVariable(1, 2) === g1)
      }
      it("maps g1.assignVariable(2,3) to g1a") {
        assert(g1.assignVariable(2, 3) === g1a)
      }
    }

    describe("The assignFieldToVariable method") {
      it("gives bottom when the src variable is definitively null") {
        assert(g1.assignFieldToVariable(2, 3, 'a').isBottom)
      }
      it("maps g1.assignFieldToVariable(3, 2, 'b') to g1b") {
      //  assert(g1.assignFieldToVariable(3, 2, 'b') === g1b)
      }
    }
  }
}
