package chester.utils.elab

case class WaitingConstraint(vars: Vector[CellAny], x: Constraint) {
  def related(x: CellAny): Boolean = vars.contains(x)
}
