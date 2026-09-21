import re

with open('self-hosted/codegen_go.chester', 'r') as f:
    content = f.read()

# 1. Modify go_return to handle ret_ty == ""
content = content.replace(
'''def go_return(sigs: List(Pair), locals: List(Pair), ret_ty: String, e_ast: AST.AST, e_str: String): String = {
  string_concat("return ", go_wrap_ret(sigs, locals, ret_ty, e_ast, e_str))
};''',
'''def go_return(sigs: List(Pair), locals: List(Pair), ret_ty: String, e_ast: AST.AST, e_str: String): String = {
  if string_eq(ret_ty, "") then string_concat(e_str, ";")
  else string_concat("return ", go_wrap_ret(sigs, locals, ret_ty, e_ast, e_str))
};''')

# 2. Modify emit_go_stmt AstRef case
content = content.replace(
'''      case AstRef(name) => {
        if string_eq(name, "Unit") then "" else go_return(sigs, locals, ret_ty, ast, name)
      };''',
'''      case AstRef(name) => {
        if string_eq(name, "Unit") then {
          if string_eq(ret_ty, "") then "" else "return nil;"
        } else {
          if string_eq(ret_ty, "") then "" else go_return(sigs, locals, ret_ty, ast, name)
        }
      };''')

# 3. Modify emit_go_stmt AstBlock tail Unit case
content = content.replace(
'''          case AstRef(u) => {
            if string_eq(u, "Unit") then join_strings(go_nl(), stmts_strs)
            else {''',
'''          case AstRef(u) => {
            if string_eq(u, "Unit") then {
              let s = join_strings(go_nl(), stmts_strs);
              if string_eq(ret_ty, "") then s
              else if int_eq(list_length(stmts), 0) then "return nil;"
              else string_concat(s, string_concat(go_nl(), "return nil;"))
            } else {''')

# 4. Modify emit_go_expr_stmt to pass "" instead of go_iface()
content = content.replace(
'''      case AstBlock(_, _) => emit_go_stmt(next_fuel, sigs, locals, go_iface(), ast);
      case AstDef(_, _, _, _, _) => emit_go_stmt(next_fuel, sigs, locals, go_iface(), ast);
      case AstEnum(_, _, _) => emit_go_stmt(next_fuel, sigs, locals, go_iface(), ast);
      case AstExtension(_, _, _, _) => emit_go_stmt(next_fuel, sigs, locals, go_iface(), ast);
      case AstImport(_, _, _, _) => emit_go_stmt(next_fuel, sigs, locals, go_iface(), ast);
      case AstRecord(_, _, _) => emit_go_stmt(next_fuel, sigs, locals, go_iface(), ast);''',
'''      case AstBlock(_, _) => emit_go_stmt(next_fuel, sigs, locals, "", ast);
      case AstDef(_, _, _, _, _) => emit_go_stmt(next_fuel, sigs, locals, "", ast);
      case AstEnum(_, _, _) => emit_go_stmt(next_fuel, sigs, locals, "", ast);
      case AstExtension(_, _, _, _) => emit_go_stmt(next_fuel, sigs, locals, "", ast);
      case AstImport(_, _, _, _) => emit_go_stmt(next_fuel, sigs, locals, "", ast);
      case AstRecord(_, _, _) => emit_go_stmt(next_fuel, sigs, locals, "", ast);''')

# 5. Modify emit_go_top to pass "" instead of go_iface()
content = content.replace(
'''      emit_go_stmt(fuel, sigs, list_empty[Pair](), go_iface(), ast)''',
'''      emit_go_stmt(fuel, sigs, list_empty[Pair](), "", ast)''')
content = content.replace(
'''    case _ => emit_go_stmt(256, list_empty[Pair](), list_empty[Pair](), go_iface(), ast)''',
'''    case _ => emit_go_stmt(256, list_empty[Pair](), list_empty[Pair](), "", ast)''')

with open('self-hosted/codegen_go.chester', 'w') as f:
    f.write(content)
print("Patched!")
