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

package it.unich.jandom.targets

import soot._
import it.unich.jandom.domains.ObjectModelSuite
import it.unich.jandom.targets.jvmsoot.SootObjectModel
import it.unich.jandom.targets.jvmsoot.SootClassReachableAnalysis
import org.scalatest.FunSpec

class SootObjectModelSuite extends FunSpec with ObjectModelSuite with SootTests {
  scene.loadClassAndSupport("javatest.S3").setApplicationClass()
  scene.loadClassAndSupport("javatest.ListA").setApplicationClass()
  scene.loadClassAndSupport("javatest.Pair").setApplicationClass()
  scene.loadClassAndSupport("javatest.K").setApplicationClass()

  val cra = new SootClassReachableAnalysis(scene)
  val om = new SootObjectModel(cra)

  val classA = RefType.v("javatest.A")
  val classB = RefType.v("javatest.B")
  val classS1 = RefType.v("javatest.S1")
  val classS2 = RefType.v("javatest.S2")
  val classS3 = RefType.v("javatest.S3")
  val classK = RefType.v("javatest.K")

  val classPair = RefType.v("javatest.Pair")
  val classListA = RefType.v("javatest.ListA")
  val classObject = RefType.v("java.lang.Object")
  val interfaceList = RefType.v("javatest.ListInterface")
  val primitiveInt = IntType.v()
  val primitiveByte = ByteType.v()
  val someTypes = Seq(classA, classB, classListA, classPair, classS2, classS3, classObject, interfaceList,
       primitiveInt, primitiveByte, classK)

  describe("it passes the following tests") {
    it("Classes have correct number of fields") {
      assert ( om.fieldsOf(classPair).size === 2 )
      assert ( om.fieldsOf(classS2).size === 1 )
      assert ( om.fieldsOf(classS3).size === 2 )
      assert ( om.fieldsOf(primitiveInt).size === 0 )
      assert ( om.fieldsOf(primitiveByte).size === 0 )
      assert ( om.fieldsOf(classA).size === 0 )
    }
    it("Subype relation is correct") {
      assert ( om.lteq(classS3,classS1) )
      assert ( om.lteq(classS2,classS1) )
      assert ( om.lteq(classS3,classS2) )
      assert ( om.lteq(classListA,interfaceList) )
    }
    it("Fields have the right types") {
      assert ( om.fieldsOf(classPair).map { om.typeOf(_) } === Set(classA,classB) )
      assert ( om.fieldsOf(classS3).map { om.typeOf(_) } === Set(classA,classB) )
    }
    it("Field of the same name in different classes are different") {
      val f1 = classPair.getSootClass().getField("v",classA)
      val f2 = classListA.getSootClass().getField("v",classA)
      assert (f1 != f2)
      assert ( (om.fieldsOf(classPair) intersect om.fieldsOf(classListA)).isEmpty )
    }
    it("Possible sharing information is correct") {
      assert (om.mayShare(classA, classA))
      assert (! om.mayShare(classA, classK))
      assert (om.mayShare(classA, classListA))
      assert (om.mayShare(classA, classPair))
      assert (om.mayShare(classB, classPair))
      assert (om.mayShare(classS2, classA))
      assert (om.mayShare(classS3, classA))
      assert (om.mayShare(classA, classS2))
      assert (om.mayShare(classA, classS3))
      // TODO deal with interfaces
      // assert (om.mayShare(interfaceList, interfaceList))
      // assert (om.mayShare(interfaceList, classListA))
    }
  }
}

