package it.unich.jandom.fixpoint

abstract class TwoPhaseSolver[EQS <: EquationSystem](val eqs: EQS, val upsolver: FixpointSolver[EQS]) extends FixpointSolver[EQS] {

  val name = "Two phase (widening/narrowing) solver" + upsolver.name

  def apply(start: eqs.Assignment, boxes: eqs.BoxAssignment): eqs.Assignment
}
