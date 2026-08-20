sed -i '' '/| AstError e => TsThrow e/c\
  | AstError e => TsThrow e\
  | AstSpan _ inner => emit_ts inner\
' theories/Backend.v

sed -i '' '/| AstError e => GoPanic e/c\
  | AstError e => GoPanic e\
  | AstSpan _ inner => emit_go inner\
' theories/Backend.v
