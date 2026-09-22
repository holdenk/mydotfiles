#!/usr/bin/env python3
"""Deny recursive force-rm. Mirrors ~/.claude/settings.json Bash(rm -rf:*) denies."""
from __future__ import annotations

import json
import re
import sys

RM = re.compile(r"(?:^|[\n;&|]|&&|\|\|)\s*rm\b")


def command_from_stdin() -> str:
    raw = sys.stdin.read()
    if not raw.strip():
        return ""
    data = json.loads(raw)
    if isinstance(data.get("tool_input"), dict):
        return data["tool_input"].get("command") or data.get("command") or ""
    return data.get("command") or ""


def is_recursive_force_rm(command: str) -> bool:
    if not RM.search(command):
        return False
    return bool(
        re.search(r"\s-[^\s]*[rR][^\s]*[fF]\b", command)
        or re.search(r"\s-[^\s]*[fF][^\s]*[rR]\b", command)
        or re.search(r"\s-[rR]\b[^\n;&|]*\s-[fF]\b", command)
        or re.search(r"\s-[fF]\b[^\n;&|]*\s-[rR]\b", command)
    )


def main() -> None:
    command = command_from_stdin()
    if is_recursive_force_rm(command):
        msg = "Blocked recursive force-rm. Do not rm -rf from the agent."
        print(json.dumps({"permission": "deny", "user_message": msg, "agent_message": msg}))
        return
    print(json.dumps({"permission": "allow"}))


if __name__ == "__main__":
    main()
