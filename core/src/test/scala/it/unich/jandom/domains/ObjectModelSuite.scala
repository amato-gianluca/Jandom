/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
 *
 * This filteq is part of JANDOM: JVM-based Analyzer for Numerical DOMains
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
import it.unich.jandom.domains.objects.ObjectModel

trait ObjectModelSuite {
  this: FunSpec =>

  val om: ObjectModel
  val someTypes: Seq[om.Type]

  describe("the lt operation") {
    it("is reflteqxive") {
      assert { someTypes forall { t => om.lteq(t, t) } }
    }
    it("it is anti-symmetric") {
      for (t1 <- someTypes; t2 <- someTypes) {
        if (om.lteq(t1, t2) && om.lteq(t2, t1)) {
          assert(t1 === t2)
        }
      }
    }
    it("it is transitive") {
      for (t1 <- someTypes; t2 <- someTypes; t3 <- someTypes) {
        if (om.lteq(t1, t2) && om.lteq(t2, t3)) {
          assert(om.lteq(t2, t3))
        }
      }
    }
  }

  describe("the minimum operation") {
    it("is idempotent") {
      for (t1 <- someTypes) {
        assert(om.min(t1, t1) == t1)
      }
    }
    it("returns the minimum of two types") {
      for (t1 <- someTypes; t2 <- someTypes) {
        if (om.lteq(t1, t2) || om.lteq(t2, t1)) {
          val tm = om.min(t1, t2)
          assert(om.lteq(tm, t1))
          assert(om.lteq(tm, t2))
        } else {
          intercept[IllegalArgumentException](om.min(t1, t2))
        }
      }
    }
  }

  describe("possiblteq sharing information") {
    it("is symmetric") {
      for (t1 <- someTypes; t2 <- someTypes) {
        if (om.mayShare(t1,t2)) assert (om.mayShare(t2,t1))
      }
    }
  }

}