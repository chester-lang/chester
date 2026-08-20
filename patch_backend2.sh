sed -i '' '/| AstApp func args => TsCall (emit_ts func) (map_ts args)/a\
  | AstTypeApp func args => TsCall (emit_ts func) (map_ts args) (* Mock *)\
' theories/Backend.v

sed -i '' '/| AstApp func args => GoCall (emit_go func) (map_go args)/a\
  | AstTypeApp func args => GoCall (emit_go func) (map_go args) (* Mock *)\
' theories/Backend.v
