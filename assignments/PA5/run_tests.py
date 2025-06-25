#!/usr/bin/env python3
import subprocess
import re

TESTS = [
    {"sources": ["arith"], "input": "arith.input"},
    {"sources": ["atoi_test", "atoi"], "input": "atoi_test.input"},
    {"sources": ["book_list"]},
    {"sources": ["cells"]},
    {"sources": ["complex"]},
    {"sources": ["cool"]},
    {"sources": ["graph"], "input": "g1.graph"},
    {"sources": ["hairyscary"]},
    {"sources": ["hello_world"]},
    {"sources": ["io"]},
    {"sources": ["lam"]},
    {"sources": ["life"], "input": "life.input"},
    {"sources": ["list"]},
    {"sources": ["new_complex"]},
    {"sources": ["palindrome"], "input": "palindrome.input"},
    {"sources": ["primes"]},
    {"sources": ["sort_list"], "input": "sort_list.input"},
]

COMPILE_FLAGS = ["-g", "-O"]

def run_command(cmd, stdin_file=None):
    try:
        if stdin_file:
            with open(stdin_file, "rb") as f:
                proc = subprocess.run(cmd, stdin=f, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False)
        else:
            proc = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False)
        return proc.returncode, proc.stdout.decode(), proc.stderr.decode()
    except Exception as e:
        return -1, "", str(e)

def extract_stats(output):
    # Remove stats lines and extract numbers
    stats_re1 = re.compile(r"Stats -- #instructions : (\d+)")
    stats_re2 = re.compile(r"#reads : (\d+)  #writes (\d+)  #branches (\d+)  #other (\d+)")
    stats = {}
    lines = output.splitlines()
    new_lines = []
    for line in lines:
        m = stats_re1.match(line.strip())
        if m:
            stats["instructions"] = int(m.group(1))
            continue
        m = stats_re2.match(line.strip())
        if m:
            stats["reads"] = int(m.group(1))
            stats["writes"] = int(m.group(2))
            stats["branches"] = int(m.group(3))
            stats["other"] = int(m.group(4))
            continue
        new_lines.append(line)
    return "\n".join(new_lines), stats

# ANSI color codes
GREEN = "\033[92m"
RED = "\033[91m"
RESET = "\033[0m"

def colorize(val_coolc, val_mycoolc, key):
    # Only colorize if both are ints
    if not isinstance(val_coolc, int) or not isinstance(val_mycoolc, int):
        return f"{val_mycoolc:>8}"
    if val_mycoolc < val_coolc:
        return f"{GREEN}{val_mycoolc:>8}{RESET}"
    else:
        return f"{RED}{val_mycoolc:>8}{RESET}"

def print_result(file, res):
    s = "OK" if res["success"] else "FAIL"
    sc = res.get("stats_coolc") or {}
    sm = res.get("stats_mycoolc") or {}
    keys = ["instructions", "reads", "writes", "branches", "other"]
    colored_stats = [
        colorize(sc.get(k, "-"), sm.get(k, "-"), k) for k in keys
    ]
    print(
        f"{file:20} {s:10} "
        f"{sc.get('instructions','-'):>8} {sc.get('reads','-'):>8} {sc.get('writes','-'):>8} {sc.get('branches','-'):>8} {sc.get('other','-'):>8} | "
        f"{colored_stats[0]} {colored_stats[1]} {colored_stats[2]} {colored_stats[3]} {colored_stats[4]}"
    )
    if not res["success"]:
        print("  Errors:", "; ".join(res["errors"]))

header = (
    f"{'File':20} {'Status':10} "
    f"{'Instr':>8} {'Reads':>8} {'Writes':>8} {'Branches':>8} {'Other':>8} | "
    f"{'Instr':>8} {'Reads':>8} {'Writes':>8} {'Branches':>8} {'Other':>8}"
)

print(header)
for test in TESTS:
    test_name = test["sources"][0]
    src_files = [f"../../examples/{src_file}.cl" for src_file in test["sources"]]
    test_input = f"../../examples/{test['input']}" if 'input' in test else None
    result = {"success": True, "errors": [], "stats_coolc": None, "stats_mycoolc": None}

    # 1. coolc
    ret, out, err = run_command(["coolc"] + COMPILE_FLAGS + src_files)
    if ret != 0 or out or err:
        result["success"] = False
        result["errors"].append("coolc failed or produced output")
        result["errors"].append(err)
        print_result(test_name, result)
        continue

    # 2. spim (coolc)
    spim_cmd = [
        "spim",
        "-keepstats",
        "-ldata", "20000000",
        "-lstack", "20000000",
        "-trap_file", "../../lib/trap.handler.silent",
        "-file", f"../../examples/{test_name}.s"
    ]
    ret, out, err = run_command(spim_cmd, stdin_file=test_input)
    if ret != 0:
        result["success"] = False
        result["errors"].append("spim (coolc) failed")
        result["errors"].append(err)
        print_result(test_name, result)
        continue
    out_clean, stats = extract_stats(out)
    result["coolc_stdout"] = out_clean
    result["coolc_stderr"] = err
    result["stats_coolc"] = stats

    # 3. mycoolc
    ret, out, err = run_command(["./mycoolc"] + COMPILE_FLAGS + src_files)
    if ret != 0 or out or err:
        result["success"] = False
        result["errors"].append("mycoolc failed or produced output")
        result["errors"].append(err)
        print_result(test_name, result)
        continue

    # 4. spim (mycoolc)
    ret, out, err = run_command(spim_cmd, stdin_file=test_input)
    if ret != 0:
        result["success"] = False
        result["errors"].append("spim (mycoolc) failed")
        result["errors"].append(err)
        print_result(test_name, result)
        continue
    out_clean, stats = extract_stats(out)
    result["mycoolc_stdout"] = out_clean
    result["mycoolc_stderr"] = err
    result["stats_mycoolc"] = stats

    # 5. Compare outputs
    if result["mycoolc_stdout"] != result["coolc_stdout"] or result["mycoolc_stderr"] != result["coolc_stderr"]:
        result["success"] = False
        result["errors"].append("Output mismatch between coolc and mycoolc")

    print_result(test_name, result)
