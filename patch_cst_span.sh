cat << 'INNER' >> theories/CST.v

Definition get_span (c : CST) : Span :=
  match c with
  | Symbol _ span => span
  | Tuple _ span => span
  | ListLiteral _ span => span
  | Block _ _ span => span
  | StringLiteral _ span => span
  | IntegerLiteral _ span => span
  | SeqOf _ span => span
  | LetCST _ _ _ span => span
  | IfCST _ _ _ span => span
  | DefCST _ _ _ _ _ span => span
  | EnumCST _ _ _ span => span
  | MatchCST _ _ span => span
  | RecordCST _ _ _ span => span
  | FieldAccessCST _ _ span => span
  | Comment _ span => span
  | Error _ span => span
  end.
INNER
