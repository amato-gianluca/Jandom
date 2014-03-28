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

package it.unich.sci.jandom.domains

import org.scalatest.FunSpec
import it.unich.sci.jandom.domains.objects.ALPsDomain

trait ALPsDomainSuiteParameters {
  import scala.language.implicitConversions
  import ALPsDomain._
  
  val om = ObjectDomainSuite.TestObjectModel
  val dom = ALPsDomain(om)
  val someFibers: Seq[Seq[om.Type]] = Seq(Seq(om.tsuper, om.tsuper), Seq(om.tsuper, om.tsuper, om.tsuper), Seq(om.tsuper, om.tsuper, om.tsuper, om.tsuper))

  implicit def sizeToTypes(size: Int) = Seq.fill(size)(om.tsuper)

  val bot4 = dom.bottom(4)
  val top4 = dom.top(4)
  val g1 = {
    val n0 = Node()
    val n1 = Node()
    val n2 = Node()
    dom(Seq(Some(n0), Some(n1), Some(n1), None), Seq((n0, 'a', n2), (n1, 'b', n2)), 4)
  }
  val someProperties = Seq(bot4, top4) 
}

/**
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class ALPsDomainSuite extends FunSpec with ALPsDomainSuiteParameters with ObjectDomainSuite {
    
}
