package chester.utils.elab

import chester.utils.cell.*

// Top-level Cell trait that all solver-specific cells must implement
// This allows user code to define constraints using Cell without knowing the solver
trait Cell[+A, -B, +C <: CellContent[A, B]]

// Type aliases for convenience
type CellOf[A, B] = Cell[A, B, CellContent[A, B]]
type CellRW[T] = Cell[T, T, CellContent[T, T]]
type CellAny = Cell[Any, Nothing, CellContent[Any, Nothing]]
type CellR[+T] = Cell[T, Nothing, CellContentR[T]]
type CellW[-T] = Cell[Any, T, CellContent[Any, T]]

// Specific cell types
type OnceCell[T] = Cell[T, T, OnceCellContent[T]]
type MutableCell[T] = Cell[T, T, MutableCellContent[T]]
type CollectionCell[A] = Cell[Seq[A], Seq[A], CollectionCellContent[A, A]]
type MapCell[K, V] = Cell[Map[K, V], Map[K, V], MappingCellContent[K, V]]
type LiteralCell[T] = Cell[T, Nothing, LiteralCellContent[T]]
