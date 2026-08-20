sed -i '' 's/-> AST/-> Span -> AST/g' theories/AST.v
sed -i '' 's/MetaId -> Span -> AST/MetaId -> AST/g' theories/AST.v
sed -i '' 's/string -> Span -> AST./string -> Span -> AST./g' theories/AST.v
