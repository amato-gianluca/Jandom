/**
 * Copyright 2013, 2014 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * The test suite for `SootTypeReachableAnalysis`.
 * @author Gianluca Amato <gamato@unich.it>
 */
package it.unich.jandom.targets

import org.scalatest.FunSpec
import it.unich.jandom.targets.jvmsoot.SootTypeReachableAnalysis
import soot._
import org.scalatest.PrivateMethodTester

class SootTypeReachableAnalysisSuite extends FunSpec with PrivateMethodTester {
  val scene = Scene.v()

  val klassA = RefType.v(scene.loadClassAndSupport("javatest.A"))
  val klassB = RefType.v(scene.loadClassAndSupport("javatest.B"))
  val klassListA = RefType.v(scene.loadClassAndSupport("javatest.ListA"))
  val interfaceList = RefType.v(scene.loadClassAndSupport("javatest.ListInterface"))
  val interfaceList2 = RefType.v(scene.loadClassAndSupport("javatest.ListInterface2"))
  val interfaceList3 = RefType.v(scene.loadClassAndSupport("javatest.ListInterface3"))
  val interfaceOther = RefType.v(scene.loadClassAndSupport("javatest.OtherInterface"))

  val klassPair = RefType.v(scene.loadClassAndSupport("javatest.Pair"))

  val klassS1 = RefType.v(scene.loadClassAndSupport("javatest.S1"))
  val klassS2 = RefType.v(scene.loadClassAndSupport("javatest.S2"))
  val klassS3 = RefType.v(scene.loadClassAndSupport("javatest.S3"))
  val klassS4 = RefType.v(scene.loadClassAndSupport("javatest.S4"))
  val klassS5 = RefType.v(scene.loadClassAndSupport("javatest.S5"))

  val klassR3 = RefType.v(scene.loadClassAndSupport("javatest.R3"))

  val types = Seq(klassA, klassB, klassListA, klassPair, klassS1, klassS2, klassS3, klassS4,
    klassS5, klassR3, interfaceList, interfaceList, interfaceList2, interfaceList3, interfaceOther)

  val fh = scene.getOrMakeFastHierarchy()
  val analysis = new SootTypeReachableAnalysis(scene)

  describe("Crown type reachability") {
    val reachablesFrom = PrivateMethod[Set[Type]]('reachablesFrom)

    it("contains itself") {
      for (t <- types) {
        assert((analysis invokePrivate reachablesFrom(t)) contains t)
      }
    }

    it("is transitive") {
      for (t1 <- types; t2 <- types; t3 <- types) {
        val reach1 = analysis invokePrivate reachablesFrom(t1)
        val reach2 = analysis invokePrivate reachablesFrom(t2)
        if ((reach1 contains t2) && (reach2 contains t3)) {
          assert((analysis invokePrivate reachablesFrom(t1)) contains t3)
        }
      }
    }

    it("passes some specific tests") {
      expectResult(Set(klassA))(analysis invokePrivate reachablesFrom(klassA))
      expectResult(Set(klassB))(analysis invokePrivate reachablesFrom(klassB))
      expectResult(Set(klassA, klassListA))(analysis invokePrivate reachablesFrom(klassListA))
      expectResult(Set(klassA, klassB, klassPair))(analysis invokePrivate reachablesFrom(klassPair))
      expectResult(Set(klassS3, klassA, klassB))(analysis invokePrivate reachablesFrom(klassS3))
      expectResult(Set(klassS2, klassA, klassB, klassListA))(analysis invokePrivate reachablesFrom(klassS2))
      expectResult(Set(klassR3, klassS3, klassA, klassB))(analysis invokePrivate reachablesFrom(klassR3))
    }
  }

  describe("Type reachability") {

    it("is reflexive") {
      for (t <- types) {
        assert(analysis.reachableFrom(t, t))
      }
    }

    it("is transitive") {
      for (t1 <- types; t2 <- types; t3 <- types) {
        if (analysis.reachableFrom(t1, t2) && analysis.reachableFrom(t2, t3)) {
          assert(analysis.reachableFrom(t1, t3))
        }
      }
    }

    it("is downward closed on the target") {
      for (t1 <- types; t2 <- types; t3 <- types) {
        if (analysis.reachableFrom(t1, t2) && fh.canStoreType(t3, t2)) {
          assert(analysis.reachableFrom(t1, t3))
        }
      }
    }

    it("is upward closed on the source") {
      for (t1 <- types; t2 <- types; t3 <- types) {
        if (analysis.reachableFrom(t1, t2) && fh.canStoreType(t1, t3)) {
          assert(analysis.reachableFrom(t3, t2))
        }
      }
    }

    it("passes some specific tests") {
      assert(analysis.reachableFrom(klassA, klassA))
      assert(!analysis.reachableFrom(klassA, klassB))
      assert(analysis.reachableFrom(klassListA, klassA))
      assert(analysis.reachableFrom(klassR3, klassS5))
      assert(!analysis.reachableFrom(klassR3, klassS4))
      assert(analysis.reachableFrom(klassR3, klassS3))
      assert(!analysis.reachableFrom(klassR3, klassS2))
    }
  }

  describe("Type sharing") {
    it("is reflexive") {
      for (t <- types) {
        assert(analysis.mayShare(t, t))
      }
    }

    it("is symmetric") {
      for (t1 <- types; t2 <- types; if analysis.mayShare(t1,t2)) {
        assert(analysis.mayShare(t2, t1))
      }
    }

    it("is upward closed") {
      for (t1 <- types; t2 <- types; t3 <- types; if analysis.mayShare(t1, t2)) {
        if (fh.canStoreType(t2, t3)) assert(analysis.mayShare(t1, t3), s"${t1} share with ${t2} and ${t1} <= ${t3}")
        if (fh.canStoreType(t1, t3)) assert(analysis.mayShare(t2, t3), s"${t1} share with ${t2} and ${t2} <= ${t3}")
      }
    }

    it("passes some specific tests") {
      assert(analysis.mayShare(klassA, klassA))
      assert(!analysis.mayShare(klassA, klassB))
      assert(analysis.mayShare(klassA, klassListA))
      assert(!analysis.mayShare(klassB, klassListA))
      assert(analysis.mayShare(klassA, klassPair))
      assert(analysis.mayShare(klassListA, klassPair))
      assert(analysis.mayShare(klassB, klassPair))
    }
  }

}
