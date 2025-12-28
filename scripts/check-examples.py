#!/usr/bin/env python3
import argparse
import difflib
import os
import shlex
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


def run_sbt(command: str, sbt_cmd: str) -> None:
    cmd = shlex.split(sbt_cmd) + [command]
    subprocess.run(cmd, check=True)


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def write_text(path: Path, content: str) -> None:
    path.write_text(content, encoding="utf-8")


def diff_text(expected: str, actual: str, expected_name: str, actual_name: str) -> str:
    return "".join(
        difflib.unified_diff(
            expected.splitlines(True),
            actual.splitlines(True),
            fromfile=expected_name,
            tofile=actual_name,
        )
    )


def check_outputs(lang: str, src_dir: Path, out_dir: Path, ext: str, update: bool) -> bool:
    if not src_dir.is_dir():
        print(f"Expected examples directory not found: {src_dir}", file=sys.stderr)
        return False

    ok = True
    sources = sorted(src_dir.glob("*.chester"))
    for src in sources:
        base = src.stem
        expected = src_dir / f"{base}.{ext}"
        actual = out_dir / f"{base}.{ext}"

        if not actual.is_file():
            print(f"Missing generated output for {src}", file=sys.stderr)
            ok = False
            continue

        if expected.is_file():
            expected_text = read_text(expected)
            actual_text = read_text(actual)
            if expected_text != actual_text:
                if update:
                    write_text(expected, actual_text)
                    print(f"Updated {expected}")
                else:
                    print(f"Mismatch for {expected}", file=sys.stderr)
                    diff = diff_text(expected_text, actual_text, str(expected), str(actual))
                    if diff:
                        print(diff, file=sys.stderr)
                    ok = False
        else:
            if update:
                shutil.copy2(actual, expected)
                print(f"Created {expected}")
            else:
                print(f"Missing expected file {expected}", file=sys.stderr)
                ok = False

    if ok:
        print(f"{lang} example check passed.")
    else:
        print(f"{lang} example check failed.", file=sys.stderr)
    return ok


def main() -> int:
    parser = argparse.ArgumentParser(description="Check generated examples.")
    parser.add_argument("--update", dest="update", action="store_true", help="Update expected outputs in-place.")
    parser.add_argument("--regen", dest="update", action="store_true", help="Alias for --update.")
    args = parser.parse_args()

    root_dir = Path(__file__).resolve().parents[1]
    go_examples = root_dir / "examples" / "go"
    ts_examples = root_dir / "examples" / "ts"

    sbt_cmd = os.environ.get("SBT_CMD", "sbt")

    fail = False
    with tempfile.TemporaryDirectory() as tmp_dir_str:
        tmp_dir = Path(tmp_dir_str)

        if go_examples.is_dir():
            go_out = tmp_dir / "go-out"
            go_out.mkdir(parents=True, exist_ok=True)
            go_sigs = root_dir / "go-signatures.json"
            go_sigs_arg = f" --go-sigs {go_sigs}" if go_sigs.is_file() else ""
            run_sbt(f"cliJVM/run go {go_examples} --output {go_out}{go_sigs_arg}", sbt_cmd)
            if not check_outputs("Go", go_examples, go_out, "go", args.update):
                fail = True

        if ts_examples.is_dir():
            ts_out = tmp_dir / "ts-out"
            ts_out.mkdir(parents=True, exist_ok=True)
            run_sbt(f"cliJVM/run ts {ts_examples} --output {ts_out}", sbt_cmd)
            if not check_outputs("TypeScript", ts_examples, ts_out, "ts", args.update):
                fail = True

    if fail:
        print("Example check failed.", file=sys.stderr)
        return 1
    print("Example check passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
