sed -i '' '/| AstError msg => TsThrow msg/c\
  | AstError msg => TsThrow msg\
  | AstSpan _ inner => emit_ts inner\
' theories/Backend.v

sed -i '' '/| AstError msg => GoPanic msg/c\
  | AstError msg => GoPanic msg\
  | AstSpan _ inner => emit_go inner\
' theories/Backend.v
