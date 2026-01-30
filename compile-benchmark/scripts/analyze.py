#!/usr/bin/env python3
"""
Analyze compile time benchmark results and generate summary report.
"""

import csv
import os
import statistics
from collections import defaultdict
from pathlib import Path


def load_loc_stats(filepath):
    """Load LOC statistics from CSV."""
    stats = {}
    if not os.path.exists(filepath):
        return stats

    with open(filepath, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            key = (row["language"], row["project"])
            stats[key] = {
                "size": row["size_category"],
                "files": int(row["files"]),
                "lines": int(row["lines"]),
                "code": int(row["code"]),
                "comments": int(row["comments"]),
                "blanks": int(row["blanks"]),
            }
    return stats


def load_timing_files(results_dir):
    """Load all timing CSV files."""
    timings = defaultdict(list)
    timing_dir = results_dir / "timings"

    if not timing_dir.exists():
        return timings

    for filepath in timing_dir.glob("*_times.csv"):
        lang = filepath.stem.replace("_times", "")
        with open(filepath, newline="") as f:
            reader = csv.DictReader(f)
            for row in reader:
                if row.get("success", "1") == "1":
                    key = (lang, row["project"], row["mode"])
                    timings[key].append(float(row["time_sec"]))

    return timings


def compute_timing_stats(timings):
    """Compute mean and std for each project/mode combination."""
    stats = {}
    for key, times in timings.items():
        if len(times) > 0:
            mean = statistics.mean(times)
            std = statistics.stdev(times) if len(times) > 1 else 0.0
            stats[key] = {"mean": mean, "std": std, "runs": len(times)}
    return stats


def merge_results(loc_stats, timing_stats):
    """Merge LOC and timing stats into unified records."""
    results = []
    seen = set()

    for (lang, project, mode), timing in timing_stats.items():
        loc = loc_stats.get((lang, project), {})
        code_lines = loc.get("code", 0)

        loc_per_sec = code_lines / timing["mean"] if timing["mean"] > 0 else 0

        record = {
            "language": lang,
            "project": project,
            "size": loc.get("size", "unknown"),
            "files": loc.get("files", 0),
            "code_lines": code_lines,
            "mode": mode,
            "mean_time": timing["mean"],
            "std_time": timing["std"],
            "runs": timing["runs"],
            "loc_per_sec": loc_per_sec,
        }
        results.append(record)
        seen.add((lang, project))

    return results


def generate_summary(results, output_path, hardware_info_path):
    """Generate markdown summary report."""
    # Group by language and size
    by_lang = defaultdict(list)
    by_lang_size = defaultdict(lambda: defaultdict(list))

    for r in results:
        by_lang[r["language"]].append(r)
        by_lang_size[r["language"]][r["size"]].append(r)

    # Read hardware info
    hw_info = ""
    if os.path.exists(hardware_info_path):
        with open(hardware_info_path) as f:
            hw_info = f.read()

    with open(output_path, "w") as f:
        f.write("# Compile Time Benchmark Results\n\n")

        # Hardware section
        if hw_info:
            f.write("## System Information\n\n")
            f.write("```\n")
            f.write(hw_info)
            f.write("```\n\n")

        # Summary statistics by language
        f.write("## Summary by Language\n\n")
        f.write("| Language | Projects | Total LOC | Avg LOC/sec (release) | Avg Build Time (s) |\n")
        f.write("|----------|----------|-----------|----------------------|--------------------|\n")

        for lang in ["fortran", "c", "cpp", "zig", "go", "rust"]:
            records = [r for r in by_lang[lang] if r["mode"] == "release"]
            if not records:
                continue

            total_loc = sum(r["code_lines"] for r in records)
            avg_loc_per_sec = (
                statistics.mean([r["loc_per_sec"] for r in records if r["loc_per_sec"] > 0])
                if any(r["loc_per_sec"] > 0 for r in records)
                else 0
            )
            avg_time = statistics.mean([r["mean_time"] for r in records])

            f.write(
                f"| {lang:8} | {len(records):8} | {total_loc:>9,} | "
                f"{avg_loc_per_sec:>20,.0f} | {avg_time:>18.2f} |\n"
            )

        f.write("\n")

        # Detailed results by language and size
        f.write("## Detailed Results\n\n")

        for lang in ["fortran", "c", "cpp", "zig", "go", "rust"]:
            lang_records = by_lang[lang]
            if not lang_records:
                continue

            f.write(f"### {lang.upper()}\n\n")

            for size in ["small", "medium", "large"]:
                size_records = [r for r in by_lang_size[lang][size]]
                if not size_records:
                    continue

                f.write(f"#### {size.capitalize()} Projects\n\n")
                f.write("| Project | LOC | Debug (s) | Release (s) | LOC/sec |\n")
                f.write("|---------|-----|-----------|-------------|--------|\n")

                # Group by project
                projects = {}
                for r in size_records:
                    if r["project"] not in projects:
                        projects[r["project"]] = {"loc": r["code_lines"]}
                    projects[r["project"]][r["mode"]] = r["mean_time"]
                    if r["mode"] == "release":
                        projects[r["project"]]["loc_per_sec"] = r["loc_per_sec"]

                for proj, data in sorted(projects.items()):
                    debug_time = data.get("debug", 0)
                    release_time = data.get("release", 0)
                    loc_per_sec = data.get("loc_per_sec", 0)

                    f.write(
                        f"| {proj[:25]:25} | {data['loc']:>5,} | "
                        f"{debug_time:>9.2f} | {release_time:>11.2f} | "
                        f"{loc_per_sec:>7,.0f} |\n"
                    )

                f.write("\n")

        # Build success/failure summary
        f.write("## Build Results\n\n")
        f.write("| Language | Successful | Failed | Success Rate |\n")
        f.write("|----------|------------|--------|-------------|\n")

        for lang in ["fortran", "c", "cpp", "zig", "go", "rust"]:
            records = by_lang[lang]
            if not records:
                continue
            success = len(records)
            # Note: failed builds are not in results
            f.write(f"| {lang:8} | {success:>10} | - | - |\n")

        f.write("\n")

    print(f"Summary written to: {output_path}")


def save_combined_csv(results, output_path):
    """Save combined results to CSV."""
    fieldnames = [
        "language",
        "project",
        "size",
        "files",
        "code_lines",
        "mode",
        "mean_time",
        "std_time",
        "runs",
        "loc_per_sec",
    ]

    with open(output_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(results)

    print(f"Combined results saved to: {output_path}")


def main():
    script_dir = Path(__file__).parent
    base_dir = script_dir.parent
    results_dir = base_dir / "results"

    print("Loading LOC statistics...")
    loc_stats = load_loc_stats(results_dir / "loc_stats.csv")
    print(f"  Loaded {len(loc_stats)} projects")

    print("Loading timing results...")
    timings = load_timing_files(results_dir)
    timing_stats = compute_timing_stats(timings)
    print(f"  Loaded {len(timing_stats)} timing records")

    print("Merging results...")
    results = merge_results(loc_stats, timing_stats)
    print(f"  Created {len(results)} combined records")

    print("Saving combined CSV...")
    save_combined_csv(results, results_dir / "compile_times.csv")

    print("Generating summary report...")
    generate_summary(
        results,
        results_dir / "summary.md",
        results_dir / "hardware_info.txt",
    )

    print("\nAnalysis complete!")


if __name__ == "__main__":
    main()
