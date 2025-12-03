#!/usr/bin/env python3
"""
compareResults.py

Usage:
    python compareResults.py [-v|--verbose] <file1.json> <file2.json>

Compares the "Compiler result" and "Runtime result" fields for each test
identified by the "Test name" key.

By default, the script prints only the names of the tests that have any
differences, sorted by file-extension priority:
    .c  → .cpp → .F90 → .F (any other extensions appear last, alphabetically).

It also prints a count of differing tests for each extension and a breakdown
by compiler name.

Use the ``--verbose`` (or ``-v``) flag to see the full detailed diff output
that was previously printed.
"""

import argparse
import json
import os
import sys
from pathlib import Path
from typing import Dict, Any, List, Tuple, Optional
from collections import Counter as _Counter
from pdb import set_trace


def load_results(path: Path) -> Dict[str, Dict[str, Any]]:
    """
    Load a JSON file that contains either:
      * a list of test dictionaries, each having a "Test name" key, or
      * a dictionary mapping test names to their result dictionaries.

    Returns a mapping from test name to the full result dictionary.
    """
    with path.open("r", encoding="utf-8") as f:
        data = json.load(f)

    if isinstance(data, dict):
        # Assume it's already a mapping of test name -> result dict
        return data

    if isinstance(data, list):
        # Build a mapping using the "Test name" field
        result: Dict[str, Dict[str, Any]] = {}
        for entry in data:
            test_name = entry.get("Test name")
            if test_name:
                result[test_name] = entry
        return result

    raise ValueError(f"Unexpected JSON structure in {path}")


def compare_fields(
    results1: Dict[str, Dict[str, Any]],
    results2: Dict[str, Dict[str, Any]],
) -> List[Tuple[str, str, Optional[Tuple[Any, Any]], Optional[Tuple[Any,
                                                                    Any]]]]:
    """
    Compare the "Compiler result" and "Runtime result" fields for each test.

    Returns a list of tuples:
        (test_name,
         reason,
         (compiler_result_file1, runtime_result_file1) or None,
         (compiler_result_file2, runtime_result_file2) or None)
    """
    diffs: List[Tuple[
        str,
        str,
        Optional[Tuple[Any, Any]],
        Optional[Tuple[Any, Any]],
    ]] = []

    all_test_names = set(results1) | set(results2)

    for name in sorted(all_test_names):
        rec1 = results1.get(name)
        rec2 = results2.get(name)

        if rec1 is None or rec2 is None:
            # Test present only in one of the files
            diffs.append((name, "Missing in one file", None, None))
            continue

        comp1 = rec1.get("Compiler result")
        comp2 = rec2.get("Compiler result")
        run1 = rec1.get("Runtime result")
        run2 = rec2.get("Runtime result")

        if comp1 != comp2 or run1 != run2:
            diffs.append(
                (name, "Different values", (comp1, run1), (comp2, run2)))

    return diffs


def _extension_priority(test_name: str) -> int:
    """
    Return a numeric priority for the file extension of ``test_name``.
    Lower numbers have higher priority in sorting.

    Priority order:
        .c   → 0
        .cpp → 1
        .F90 → 2
        .F   → 3
    Any other extension gets 99 (sorted after the known ones, then alphabetically).
    """
    _, ext = os.path.splitext(test_name)
    priority_map = {".c": 0, ".cpp": 1, ".F90": 2, ".F": 3}
    return priority_map.get(ext, 99)


def _sorted_test_names(names: List[str]) -> List[str]:
    """
    Sort test names first by extension priority, then alphabetically.
    """
    return sorted(names, key=lambda n: (_extension_priority(n), n.lower()))


def _count_by_extension(names: List[str]) -> _Counter:
    """
    Return a Counter mapping file extensions to the number of occurrences
    in ``names``. Extensions are reported exactly as they appear (including the dot).
    """
    counter: _Counter = _Counter()
    for name in names:
        _, ext = os.path.splitext(name)
        if not ext:
            ext = "<no_ext>"
        counter[ext] += 1
    return counter


def _count_by_compiler(
    names: List[str],
    results1: Dict[str, Dict[str, Any]],
    results2: Dict[str, Dict[str, Any]],
) -> _Counter:
    """
    Return a Counter mapping compiler names to the number of differing tests.

    For each test name in ``names`` we look for the ``"Compiler name"`` field.
    Preference is given to the value from ``results1``; if it is missing we fall
    back to the value from ``results2``.  Missing compiler names are counted
    under the key ``"<unknown>"``.
    """
    counter: _Counter = _Counter()
    for name in names:
        comp_name = None
        rec1 = results1.get(name)
        rec2 = results2.get(name)
        if rec1 and "Compiler name" in rec1:
            comp_name = rec1["Compiler name"]
        elif rec2 and "Compiler name" in rec2:
            comp_name = rec2["Compiler name"]
        if not comp_name:
            comp_name = "<unknown>"
        counter[comp_name] += 1
    return counter


def main() -> None:
    parser = argparse.ArgumentParser(
        description=
        "Compare Compiler and Runtime results between two JSON files.")
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Show detailed differences (default prints only test names).",
    )
    parser.add_argument("file1", help="First JSON file")
    parser.add_argument("file2", help="Second JSON file")
    args = parser.parse_args()

    file1 = Path(args.file1)
    file2 = Path(args.file2)

    try:
        results1 = load_results(file1)
        results2 = load_results(file2)
    except Exception as e:
        print(f"Error loading JSON: {e}")
        sys.exit(1)

    diffs = compare_fields(results1, results2)

    if not diffs:
        for name, data in results1.items():
            if (data['Compiler result'] == 'FAIL') or (data['Runtime result'] == 'FAIL'):
                print(name)
        print("Both json files have matching Compiler and Runtime results.")
        return

    # Extract just the test names that have any kind of difference
    diff_names = [name for name, _, _, _ in diffs]

    if args.verbose:
        # Verbose mode – print the full detailed view (same as before)
        print("Tests with differing results:")
        for name, reason, val1, val2 in diffs:
            print(f"- {name}:")
            if reason == "Missing in one file":
                if name not in results1:
                    print(f"    Present only in {file2.name}")
                else:
                    print(f"    Present only in {file1.name}")
            else:
                # val1 and val2 are guaranteed to be tuples here
                comp1, run1 = val1  # type: ignore
                comp2, run2 = val2  # type: ignore

                # Show compiler differences if they exist
                if comp1 != comp2:
                    print(
                        f"    Compiler result: {file1.name}='{comp1}' vs {file2.name}='{comp2}'"
                    )
                    # Per requirement, do NOT show runtime differences when compiler differs
                    continue

                # Compiler results are the same; show runtime differences if any
                if run1 != run2:
                    print(
                        f"    Runtime result : {file1.name}='{run1}' vs {file2.name}='{run2}'"
                    )
        # After verbose output, also show the extension and compiler summaries
        ext_counts = _count_by_extension(diff_names)
        comp_counts = _count_by_compiler(diff_names, results1, results2)

        print("\nSummary of differing tests by file extension:")
        for ext, cnt in sorted(ext_counts.items(),
                               key=lambda x: (x[0] != "", x[0])):
            print(f"  {ext or '<no_ext>'}: {cnt}")

        print("\nSummary of differing tests by compiler name:")
        for comp, cnt in sorted(comp_counts.items(),
                                key=lambda x: (x[0] != "", x[0])):
            print(f"  {comp}: {cnt}")

    else:
        # Default mode – only list the test names, sorted by extension priority
        sorted_names = _sorted_test_names(diff_names)
        print("Tests with differing results (sorted by file extension):")
        for name in sorted_names:
            print(f"- {name}")

        # Print counts per extension
        ext_counts = _count_by_extension(diff_names)
        print("\nCount of differing tests per extension:")
        for ext, cnt in sorted(ext_counts.items(),
                               key=lambda x: (x[0] != "", x[0])):
            print(f"{ext or '<no_ext>'}: {cnt}")

        # Print counts per compiler
        comp_counts = _count_by_compiler(diff_names, results1, results2)
        print("\nCount of differing tests per compiler:")
        for comp, cnt in sorted(comp_counts.items(),
                                key=lambda x: (x[0] != "", x[0])):
            print(f"{comp}: {cnt}")


if __name__ == "__main__":
    main()
