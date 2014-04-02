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

import org.scalatest.FunSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor1

import it.unich.jandom.domains.CartesianFiberedDomainSuite

/**
 * This is a common trait for test suites of object domains. It contains all the standard tests
 * valid for all object domains.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait ObjectDomainSuite extends CartesianFiberedDomainSuite with TableDrivenPropertyChecks {
  this: FunSpec =>

  val om: ObjectModel
  val dom: ObjectDomain[om.type]

  val somePropertiesAndTwoVars = Table((someProperties.heading, "v1", "v2"),
    (for (p <- someProperties; v1 <- 0 until p.dimension; v2 <- 0 until p.dimension) yield (p, v1, v2)): _*)

  val someAssignVariable = Table((someProperties.heading, "dst", "src"),
    (for (p <- someProperties; dst <- 0 until p.dimension; src <- 0 until p.dimension; if om.lteq(p.typeOf(src), p.typeOf(dst))) yield (p, dst, src)): _*)
    
  val someAssignFieldToVar = Table((someProperties.heading, "dst", "src", "f"),
    (for (p <- someProperties; dst <- 0 until p.dimension; src <- 0 until p.dimension; f <- om.fieldsOf(p.typeOf(src)); if om.lteq(om.typeOf(f), p.typeOf(dst))) yield (p, dst, src, f)): _*)

  val someAssignVarToField = Table((someProperties.heading, "dst", "f", "src"),
    (for (p <- someProperties; dst <- 0 until p.dimension; f <- om.fieldsOf(p.typeOf(dst)); src <- 0 until p.dimension; if om.lteq(p.typeOf(src), om.typeOf(f))) yield (p, dst, f, src)): _*)

  val someCast = Table((someProperties.heading, "v", someTypes.heading),
    (for (p <- someProperties; v <- 0 until p.dimension; t <- someTypes; if om.lteq(t, p.typeOf(v))) yield (p, v, t)): _*)

  describe("The mayShare method") {
    it("is true when two variables may be aliased") {
      forAll(somePropertiesAndTwoVars) { (p, v1, v2) =>
        whenever(p.mayBeAliases(v1, v2)) { assert(p.mayShare(v1, v2)) }
      }
    }
    it("is true when two variables must share") {
      forAll(somePropertiesAndTwoVars) { (p, v1, v2) =>
        whenever(p.mustShare(v1, v2)) { assert(p.mayShare(v1, v2)) }
      }
    }
  }

  describe("The assignNull method") {
    it("makes variables possibly null") {
      forAll(somePropertiesAndVars) { (p, v) =>
        assert(p.assignNull(v).mayBeNull(v))
      }
    }
  }

  describe("The assignVariable method") {
    it("makes variables possibly weak aliases") {
      forAll(someAssignVariable) { (p, dst, src) =>
          assert(p.assignVariable(dst, src).mayBeWeakAliases(dst, src))
      }
    }
  }

  describe("The assignFieldToVariable method") {
    it("makes variables possibly share if source and target are not null") {
      forAll(someAssignFieldToVar) { (p, i, j, f) =>
        whenever(!p.mustBeNull(i) && !p.mustBeNull(j, Seq(f))) {
          assert(p.assignFieldToVariable(i, j, f).mayShare(i, j))
        }
      }
    }
    it("propagate possibly nullness") {
      forAll(someAssignFieldToVar) { (p, i, j, f) =>
        whenever(p.mayBeNull(j, Seq(f))) {
          assert(p.assignFieldToVariable(i, j, f).mayBeNull(i))
        }
      }
    }
    it("propagate possibly non nullness if target is not null") {
      forAll(someAssignFieldToVar) { (p, i, j, f) =>
        whenever(!p.mustBeNull(i) && !p.mustBeNull(j, Seq(f))) {
          assert(!p.assignFieldToVariable(i, j, f).mustBeNull(i))
        }
      }
    }
  }

  describe("The assignVariableToField method") {
    it("makes variables possibly share if source and target variable are not null") {
      forAll(someAssignVarToField) { (p, i, f, j) =>
        whenever(!p.mustBeNull(i) && !p.mustBeNull(j)) {
          assert(p.assignVariableToField(i, f, j).mayShare(i, j))
        }
      }
    }
    it("propagate possibly nullness") {
      forAll(someAssignVarToField) { (p, i, f, j) =>
        whenever(p.mayBeNull(j)) {
          assert(p.assignVariableToField(i, f, j).mayBeNull(i, Seq(f)))
        }
      }
    }
    it("propagate possibly non nullness if target variable is not null") {
      forAll(someAssignVarToField) { (p, i, f, j) =>
        whenever(!p.mustBeNull(i) && !p.mustBeNull(j)) {
          assert(!p.assignVariableToField(i, f, j).mustBeNull(i, Seq(f)))
        }
      }
    }
  }

  describe("The cast method") {
    it("propagate possibly nullness") {
      forAll(someCast) { (p, i, t) =>
        whenever(p.mayBeNull(i)) {
          assert(p.castVariable(i, t).mayBeNull(i))
        }
      }
    }
    it("propagate possibly non-nullness") {
      forAll(someCast) { (p, i, t) =>
        whenever(!p.mustBeNull(i)) {
          assert(!p.castVariable(i, t).mustBeNull(i))
        }
      }
    }
  }

  describe("The addFreshVariable method") {
    it("add a variable") {
      forAll(someProperties) { (p) =>
        forAll(someTypes) { (t) =>
          assert(p.addFreshVariable(t).dimension === p.dimension + 1)
        }
      }
    }

    it("adds a variable which cannnot share and alias with other variables and cannot be null") {
      forAll(someProperties) { (p) =>
        forAll(someTypes) { (t) =>
          val p2 = p.addFreshVariable(t)
          assert(!p2.mustBeNull(p.dimension))
          assert(p2.dimension === p.dimension + 1)
          for (j <- 0 until p.dimension) {
            assert(!p2.mustShare(p.dimension, j))
            assert(!p2.mustBeAliases(p.dimension, j))
          }
        }
      }
    }
  }

  describe("The addUnknownVariable method") {
    forAll(someProperties) { (p) =>
      forAll(someTypes) { (t) =>
        assert(p.addUnknownVariable(t).dimension === p.dimension + 1)
      }
    }
  }
}

object ObjectDomainSuite {

  /**
   * This is an object model used in several tests.
   * @author Gianluca Amato <gamato@unich.it>
   */
  object TestObjectModel extends ObjectModel with ObjectModel.NoArrays {
    type Type = AnyRef
    type Field = Char

    val tother = "tother"
    val tsuper = "tsuper"
    val tsub = "tsub"

    def lteq(t1: Type, t2: Type) = t1 == this.tsub || t2 == this.tsuper
    def glbApprox(ts: Iterable[Type]) = if (ts.isEmpty)
      None
    else if (ts exists { _ == tsub })
      Some(tsub)
    else
      Some(tsuper)
    def mayShare(t1: Type, t2: Type) = true
    def mayBeAliases(t1: Type, t2: Type) = true
    def fieldsOf(t: Type) = t match {
      case `tsuper` => Set('a', 'b')
      case `tsub` => Set('a', 'b', 'c')
      case `tother` => Set()
    }
    def typeOf(f: Field) = f match {
      case 'a' => tsuper
      case 'b' => tsuper
      case 'c' => tsub
    }
  }
}
