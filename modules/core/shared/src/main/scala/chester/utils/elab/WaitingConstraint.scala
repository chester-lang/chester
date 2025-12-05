package chester.utils.elab

// Constraints can be any user-defined type
case class WaitingConstraint(vars: Vector[CellAny], constraint: Any) {
  def related(cell: CellAny): Boolean = vars.contains(cell)
}
