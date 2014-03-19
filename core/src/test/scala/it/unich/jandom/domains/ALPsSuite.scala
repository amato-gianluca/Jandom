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
import it.unich.jandom.domains.objects.ALPsDomain
import it.unich.jandom.domains.objects.ObjectModel
import org.scalatest.FlatSpec
import org.scalatest.PrivateMethodTester

/**
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class ALPsSpec extends FunSpec with PrivateMethodTester {
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
    it("is bigger than bottom") { assert(g > bottom) }
    it("is smaller than top") { assert(g < top) }
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
        case `tsuper` => Set('a', 'b')
        case `tsub` => Set('a', 'b', 'c')
        case _ => Set()
      }
      def typeOf(f: Field) = f match {
        case 'a' => tsuper
        case 'b' => tsuper
        case 'c' => tsub
        case _ => tother
      }
      def lteq(t1: Type, t2: Type) = t1 == this.tsub || t2 == this.tsuper
    }

    val dom = ALPsDomain(TrivialObjectModel)
    val om = TrivialObjectModel

    val nodeType = PrivateMethod[Option[om.Type]]('nodeType)
    val expandSpan = PrivateMethod[Map[om.Field, dom.Node]]('expandSpan)

    implicit def sizeToTypes(size: Int) = Seq.fill(size)(om.tsuper)

    val types = Seq(om.tsuper, om.tsuper, om.tsuper, om.tsub)

    val g1 = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      val n2 = new dom.Node
      dom(Seq(Some(n0), Some(n1), Some(n1), None), Seq((n0, 'a', n2), (n1, 'b', n2)), 4)
    }
    val g1a = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      val n2 = new dom.Node
      dom(Seq(Some(n2), Some(n1), None, None), Seq((n1, 'b', n0), (n2, 'a', n0)), 4)
    }
    val g1b = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      val n2 = new dom.Node
      dom(Seq(Some(n0), Some(n1), Some(n2), None), Seq((n0, 'a', n2), (n1, 'b', n2)), 4)
    }
    val g1c = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      val n2 = new dom.Node
      dom(Seq(Some(n0), Some(n1), Some(n1), None), Seq((n0, 'a', n2), (n0, 'c', new dom.Node), (n1, 'b', n2)), om.tsub +: 3)
    }
    val g1d = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      val n2 = new dom.Node
      val n3 = new dom.Node
      dom(Seq(Some(n0), Some(n1), Some(n1), None, Some(n3)), Seq((n0, 'a', n2), (n1, 'b', n2), (n3, 'a', new dom.Node), (n3, 'b', new dom.Node)), 5)
    }
    val g1e = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      val n2 = new dom.Node
      dom(Seq(Some(n1), Some(n0), None), Seq((n0, 'a', n2), (n1, 'b', n2)), 3)
    }
    val g1f = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      val n2 = new dom.Node
      dom(Seq(Some(n0), Some(n1), None), Seq((n0, 'a', n2), (n1, 'b', n2)), 3)
    }
    val g1bb = {
      dom(Seq(None, Some(new dom.Node), None, None), Seq(), 4)
    }
    val g2 = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      val n2 = new dom.Node
      dom(Seq(Some(n0), Some(n1), Some(n1), None), Seq((n0, 'a', n2)), 4)
    }
    val g3 = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      val n2 = new dom.Node
      val n3 = new dom.Node
      dom(Seq(Some(n2), Some(n1), Some(n1), Some(n3)), Seq((n2, 'a', n0), (n2, 'b', n3)), 4)
    }
    val g4 = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      val n2 = new dom.Node
      dom(Seq(Some(n0), Some(n1), Some(n1), Some(n0)), Seq((n0, 'a', n2), (n0, 'b', n2), (n1, 'b', n0)), 3 :+ om.tsub)
    }
    val g4b = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      val n2 = new dom.Node
      dom(Seq(Some(n0), Some(n1), Some(n1), None), Seq((n0, 'a', n2)), 3 :+ om.tsub)
    }
    val g4union = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      val n2 = new dom.Node
      val n4 = new dom.Node
      dom(Seq(Some(n0), Some(n1), Some(n1), Some(n2)), Seq((n0, 'a', new dom.Node), (n0, 'b', new dom.Node), (n1, 'b', new dom.Node),
        (n2, 'a', n4), (n2, 'b', n4)), 3 :+ om.tsub)
    }
    val g4big = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      val n2 = new dom.Node
      dom(Seq(Some(n0), Some(n1), Some(n1), Some(n2)), Seq((n0, 'a', new dom.Node), (n0, 'b', new dom.Node), (n1, 'b', new dom.Node),
        (n2, 'a', new dom.Node), (n2, 'b', new dom.Node)), 3 :+ om.tsub)
    }
    val g5 = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      dom(Seq(Some(n0), Some(n1), Some(n1), None), Seq((n0, 'a', new dom.Node), (n1, 'a', n1), (n1, 'b', new dom.Node)), 4)
    }
    val g5big = {
      val n0 = new dom.Node
      val n1 = new dom.Node
      dom(Seq(Some(n0), Some(n1), Some(n1), None), Seq((n0, 'a', new dom.Node), (n1, 'a', new dom.Node), (n1, 'b', new dom.Node)), 4)
    }
    val bot4 = dom.bottom(4)
    val top4 = dom.top(4)

    val allgraphs = Seq(g1, g2, g3, g4, g5, bot4, top4, g1a, g1b, g1c, g1d, g1e, g1f, g1bb)

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

    describe("The graph g4") {
      it should behave like nonExtremalGraph(om)(dom)(g4)
      it("has dimension 4") { assert(g4.dimension === 4) }
      it("is smaller than g4big") { assert(g4 < g4big) }
    }

    describe("The graph g5") {
      it should behave like nonExtremalGraph(om)(dom)(g5)
      it("has dimension 4") { assert(g5.dimension === 4) }
      it("is smaller than g5big") { assert(g5 < g5big) }
    }

    describe("The nodeType method") {
      it("returns type t for nodes bounds to variables all of type t") {
        assert((g4 invokePrivate nodeType(g4.labelOf(1).get)) === Some(om.tsuper))
      }
      it("returns the least type of all variables bound to the node") {
        assert((g4 invokePrivate nodeType(g4.labelOf(0).get)) === Some(om.tsub))
      }
      it("returns None fot nodes not bound to variables") {
        assert((g4 invokePrivate nodeType(g4.labelOf(0, 'a').get)) === None)
      }
    }

    describe("The expandSpan method") {
      val span = g1.labelOf(0).get
      val newspan = g1 invokePrivate expandSpan(span, om.tsub)
      it("adds c field when moving from tsuper to tsub") {
        assert(newspan isDefinedAt 'c')
      }
      it("does not add null fields") {
        assert(!(newspan isDefinedAt 'b'))
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
      it("maps g1.assignFieldToVariable(2, 2, 'b') to g1b") {
        assert(g1.assignFieldToVariable(2, 2, 'b') === g1b)
      }
    }

    describe("The assignVariableToField method") {
      it("gives bottom when the dst variable is definitively null") {
        assert(g1.assignVariableToField(3, 'a',1).isBottom)
      }
      it("it maps  g1.assignVariableToField(1, 'b', 3) to g2") {
        assert(g1.assignVariableToField(1, 'b', 3) === g2)
      }
      it("it maps  g1.assignVariableToField(2, 'a', 2) to g5") {
        assert(g1.assignVariableToField(2, 'a', 2) === g5)
      }
    }

    describe("The cast method") {
      it("maps g1.castVariable(0,tsub) to g1c") {
        assert(g1.castVariable(0, om.tsub) === g1c)
      }
    }

    describe("The addFreshVariable method") {
      it("maps g1.addFreshVariable(tsuper) to g1d") {
        assert(g1.addFreshVariable(om.tsuper) === g1d)
      }
    }

    describe("The addVariable method") {
      it("transforms top in top") {
        assert(top4.addVariable(om.tsuper) === dom.top(5))
      }
    }

    describe("The delVariable method") {
      it("transforms top in top") {
        for (v <- 0 until top4.dimension) assert(top4.delVariable(v) === dom.top(3))
      }
      it("transforms bottom in bottom") {
        for (v <- 0 until bot4.dimension) assert(bot4.delVariable(v) === dom.bottom(3))
      }
    }

    describe("The mapVariables method") {
      val rhos = Seq(Seq(-1, -1, -1, 0), Seq(2, 1, -1, 0), Seq(0, 1, 2, 3), Seq(3, 0, 2, 1))
      it("transforms top in top") {
        for (v <- 0 until top4.dimension; rho <- rhos) assert(top4.mapVariables(rho).isTop)
      }
      it("transforms bottom in bottom") {
        for (v <- 0 until bot4.dimension; rho <- rhos) assert(bot4.mapVariables(rho).isBottom)
      }
      it("maps g1.mapVariables(Seq(1, -1, 0, 2)) to g1e") {
        assert(g1.mapVariables(Seq(1, -1, 0, 2)) === g1e)
      }
      it("maps g1.mapVariables(Seq(0, 1,  -1, 2)) to g1f") {
        assert(g1.mapVariables(Seq(0, 1, -1, 2)) === g1f)
      }
    }

    describe("The testNull method") {
      it("is the identity on bottom") {
        for (size <- 0 until 4; j <- 0 until size) {
          assert(dom.bottom(size).testNull(j).isBottom)
        }
      }
      it("is equivalent to assignNull for top") {
        for (size <- 0 until 4; j <- 0 until size) {
          assert(dom.top(size).testNull(j) === dom.top(size).assignNull(j))
        }
      }
      it("maps g1b.testNull(0) to g1bb") {
        assert(g1b.testNull(0) === g1bb)
      }
    }

    describe("The testNull method") {
      it("is identity on bottom") {
        for (size <- 0 until 4; j <- 0 until size) {
          assert(dom.bottom(size).testNotNull(j).isBottom)
        }
      }
      it("is identity on top") {
        for (size <- 0 until 4; j <- 0 until size) {
          assert(dom.top(size).testNotNull(j).isTop)
        }
      }
      it("is bottom if applied to a definite null variable") {
        assert(g1.testNotNull(3).isBottom)
      }
    }

    describe("The union method") {
      it("is idempotent") {
        for (g <- allgraphs) assert((g union g) === g)
      }
      it("is bigger then operands") {
        for (g1 <- allgraphs; g2 <- allgraphs; if g1.fiber == g2.fiber) {
          assert((g1 union g2) >= g2, s"${g1} union ${g2} is ${g1 union g2}")
          assert((g1 union g2) >= g1, s"${g1} union ${g2} is ${g1 union g2}")
        }
      }
      it("has bottom as neutral element") {
        for (g <- allgraphs) {
          assert((g union g.bottom) === g)
          assert((g.bottom union g) === g)
        }
      }
      it("has top as absorbing element") {
        for (g <- allgraphs) {
          assert((g union g.top) === g.top)
          assert((g.top union g) === g.top)
        }
      }
      it("yields g4union when applied to g4 union g4b") {
        assert((g4b union g4) === g4union)
        assert((g4 union g4b) === g4union)
      }
    }

    describe("The intersection method") {
     it("is idempotent") {
        for (g <- allgraphs) {
          assert((g intersection g) === g)
        }
      }
      it("is bigger then operands") {
        for (g1 <- allgraphs; g2 <- allgraphs; if g1.fiber == g2.fiber) {
          assert((g1 intersection g2) <= g2, s"${g1} intersection ${g2} is ${g1 intersection g2}")
          assert((g1 intersection g2) <= g1, s"${g1} intersection ${g2} is ${g1 intersection g2}")
        }
      }
      it("has top as neutral element") {
        for (g <- allgraphs) {
          assert((g intersection g.top) === g)
          assert((g.top intersection g) === g)
        }
      }
      it("has bottom as absorbing element") {
        for (g <- allgraphs) {
          assert((g intersection g.bottom) === g.bottom)
          assert((g.bottom intersection g) === g.bottom)
        }
      }
    }

    describe("The connect method") {
      it("passes test1") {
        val n0 = new dom.Node
        val n1 = new dom.Node
        val n2 = new dom.Node
        val g1 = dom(Seq(Some(n0), Some(n0), Some(n1)), Seq(), 3)
        val g2 = dom(Seq(Some(n2)), Seq(), 1)
        assert (g1.connect(g2,1) === g1.delVariable(2))
      }
      it("passes test2") {
        val n0 = new dom.Node
        val n1 = new dom.Node
        val n2 = new dom.Node
        val n3 = new dom.Node
        val g1 = dom(Seq(Some(n0), Some(n0), Some(n1)), Seq((n0,'a',new dom.Node)), 3)
        val g2 = dom(Seq(Some(n2),Some(n3)), Seq((n3, 'b',n2)), 2)
        val g3 = dom(Seq(Some(n0), Some(n0), Some(n3)), Seq((n0,'a',new dom.Node), (n0,'b',new dom.Node), (n3,'b',n2)), 3)
        assert (g1.connect(g2,1) === g3)
      }
    }
  }
}
