#!/usr/bin/env node
/**
 * Chester bindgen v0: TypeScript .d.ts -> Chester extern module.
 *
 * Usage:
 *   node dts2chester.mjs --package react --input file.d.ts [--output out.chester] [--filter regex]
 */

import fs from "node:fs";
import path from "node:path";
import ts from "typescript";

function usage() {
  console.error(`Usage: dts2chester.mjs --package NAME --input FILE.d.ts [--output FILE.chester] [--filter REGEX]`);
  process.exit(1);
}

function parseArgs(argv) {
  const opts = { package: "", input: "", output: "", filter: "" };
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === "--package" && argv[i + 1]) opts.package = argv[++i];
    else if (arg === "--input" && argv[i + 1]) opts.input = argv[++i];
    else if (arg === "--output" && argv[i + 1]) opts.output = argv[++i];
    else if (arg === "--filter" && argv[i + 1]) opts.filter = argv[++i];
    else usage();
  }
  if (!opts.package || !opts.input) usage();
  return opts;
}

function mapPrimitive(name) {
  switch (name) {
    case "string":
      return "String";
    case "number":
      return "Integer";
    case "boolean":
      return "Bool";
    case "void":
      return "Unit";
    case "any":
    case "unknown":
    case "never":
    case "object":
    case "undefined":
    case "null":
      return "Any";
    default:
      return null;
  }
}

function chesterType(typeNode, sourceFile) {
  if (!typeNode) return "Any";

  if (typeNode.kind === ts.SyntaxKind.StringKeyword) return "String";
  if (typeNode.kind === ts.SyntaxKind.NumberKeyword) return "Integer";
  if (typeNode.kind === ts.SyntaxKind.BooleanKeyword) return "Bool";
  if (typeNode.kind === ts.SyntaxKind.VoidKeyword) return "Unit";
  if (typeNode.kind === ts.SyntaxKind.AnyKeyword) return "Any";
  if (typeNode.kind === ts.SyntaxKind.UnknownKeyword) return "Any";
  if (typeNode.kind === ts.SyntaxKind.UndefinedKeyword) return "Any";
  if (typeNode.kind === ts.SyntaxKind.NullKeyword) return "Any";
  if (typeNode.kind === ts.SyntaxKind.ObjectKeyword) return "Any";

  if (ts.isArrayTypeNode(typeNode)) {
    return `List(${chesterType(typeNode.elementType, sourceFile)})`;
  }

  if (ts.isTypeReferenceNode(typeNode)) {
    const name = typeNode.typeName.getText(sourceFile);
    const prim = mapPrimitive(name);
    if (prim) return prim;
    if (name === "Array" && typeNode.typeArguments?.length === 1) {
      return `List(${chesterType(typeNode.typeArguments[0], sourceFile)})`;
    }
    return "Any";
  }

  if (ts.isUnionTypeNode(typeNode)) {
    const parts = typeNode.types.map((t) => chesterType(t, sourceFile));
    if (parts.includes("String")) return "String";
    if (parts.includes("Integer")) return "Integer";
    if (parts.includes("Bool")) return "Bool";
    if (parts.every((p) => p === parts[0])) return parts[0];
    return "Any";
  }

  if (ts.isFunctionTypeNode(typeNode) || ts.isTypeQueryNode(typeNode)) {
    return "Any";
  }

  const text = typeNode.getText(sourceFile).trim();
  const prim = mapPrimitive(text);
  if (prim) return prim;
  if (text.endsWith("[]")) {
    const inner = text.slice(0, -2).trim();
    const innerPrim = mapPrimitive(inner);
    return `List(${innerPrim ?? "Any"})`;
  }
  return "Any";
}

function chesterParams(params, sourceFile) {
  const out = [];
  for (const param of params) {
    if (!ts.isParameter(param)) continue;
    const name = param.name.getText(sourceFile);
    if (name.startsWith("...")) continue;
    if (param.dotDotDotToken) {
      const elem = param.type
        ? chesterType(param.type, sourceFile).replace(/^List\((.*)\)$/, "$1")
        : "Any";
      out.push([name, `List(${elem})`]);
      continue;
    }
    out.push([name, chesterType(param.type, sourceFile)]);
  }
  return out;
}

function emitFunction(name, params, retType, sourceFile) {
  const paramStr = params.map(([n, t]) => `${n}: ${t}`).join(", ");
  return `  def ${name}(${paramStr}): ${retType};`;
}

function matchesFilter(name, filter) {
  if (!filter) return true;
  return new RegExp(filter).test(name);
}

function collectDecls(sourceFile, filter) {
  const decls = [];

  for (const stmt of sourceFile.statements) {
    const isExport =
      (ts.getCombinedModifierFlags(stmt) & ts.ModifierFlags.Export) !== 0;

    if (isExport && ts.isFunctionDeclaration(stmt) && stmt.name) {
      const name = stmt.name.getText(sourceFile);
      if (!matchesFilter(name, filter)) continue;
      const params = chesterParams(stmt.parameters ?? [], sourceFile);
      const retType = stmt.type ? chesterType(stmt.type, sourceFile) : "Any";
      decls.push(emitFunction(name, params, retType, sourceFile));
      continue;
    }

    if (isExport && ts.isVariableStatement(stmt)) {
      for (const decl of stmt.declarationList.declarations) {
        if (!ts.isIdentifier(decl.name)) continue;
        const name = decl.name.getText(sourceFile);
        if (!matchesFilter(name, filter)) continue;
        if (decl.type && ts.isFunctionTypeNode(decl.type)) {
          const params = chesterParams(decl.type.parameters, sourceFile);
          const retType = chesterType(decl.type.type, sourceFile);
          decls.push(emitFunction(name, params, retType, sourceFile));
        }
      }
    }
  }

  return decls;
}

function emitChester(packageName, decls) {
  if (decls.length === 0) {
    return `extern ts "${packageName}" {\n  /* no matching declarations */\n};\n`;
  }
  return `extern ts "${packageName}" {\n${decls.join("\n")}\n};\n`;
}

function main() {
  const opts = parseArgs(process.argv.slice(2));
  const inputPath = path.resolve(opts.input);
  const sourceText = fs.readFileSync(inputPath, "utf8");
  const sourceFile = ts.createSourceFile(
    inputPath,
    sourceText,
    ts.ScriptTarget.Latest,
    true,
    ts.ScriptKind.TS
  );

  const decls = collectDecls(sourceFile, opts.filter);
  const out = emitChester(opts.package, decls);

  if (opts.output) {
    fs.writeFileSync(path.resolve(opts.output), out);
  } else {
    process.stdout.write(out);
  }
}

main();
