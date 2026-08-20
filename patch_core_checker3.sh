sed -i '' 's/| AstSpan _ a1, a2 => eq_ast a1 a2/| AstSpan _ a1, _ => false/g' theories/CoreChecker.v
sed -i '' 's/| a1, AstSpan _ a2 => eq_ast a1 a2/| _, AstSpan _ a2 => false/g' theories/CoreChecker.v
