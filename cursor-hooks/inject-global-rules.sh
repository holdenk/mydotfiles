#!/usr/bin/env python3
"""Inject alwaysApply ~/.cursor/rules/*.mdc into sessionStart additional_context."""
from __future__ import annotations

import json
import sys
from pathlib import Path

RULES = Path.home() / ".cursor" / "rules"


def frontmatter_and_body(text: str) -> tuple[str, str]:
    if not text.startswith("---"):
        return "", text
    parts = text.split("---", 2)
    if len(parts) < 3:
        return "", text
    return parts[1], parts[2].lstrip("\n")


def main() -> None:
    sys.stdin.read()
    chunks: list[str] = []
    if RULES.is_dir():
        for path in sorted(RULES.glob("**/*.mdc")):
            front, body = frontmatter_and_body(path.read_text())
            if "alwaysApply: true" in front:
                chunks.append(f"----- {path.name} -----\n{body}")
    print(json.dumps({"additional_context": "\n".join(chunks)}))


if __name__ == "__main__":
    main()
