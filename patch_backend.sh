sed -i '' '/| AstApp op args =>/a\
  | AstTypeApp op args => TsCall (emit_ts op) (map_ts args) (* Mock TypeScript Type App *)\
' theories/Backend.v

sed -i '' '/| AstApp op args =>/a\
  | AstTypeApp op args => GoCall (emit_go op) (map_go args) (* Mock Go Type App *)\
' theories/Backend.v
