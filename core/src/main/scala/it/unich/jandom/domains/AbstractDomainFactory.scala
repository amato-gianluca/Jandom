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

/**
 * A domain factory is a class which is able to build a domain. For each domain of a given
 * family (i.e., domains which takes the same parameters) we have a different subclass of
 * AbstractDomainFactory. When when need to pass a domain but we do not know the parameters,
 * we pass the domain factory instead. This is just a marker trait.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait AbstractDomainFactory
