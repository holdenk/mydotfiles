#!/usr/bin/env python3
"""Deny reads of credentials and key material. Mirrors ~/.claude/settings.json."""
from __future__ import annotations

import json
import sys
from pathlib import Path

SUFFIXES = (".pem", ".p8", ".p12", ".pfx", ".key", ".jks", ".keystore")


def deny(path: str) -> None:
    print(
        json.dumps(
            {
                "permission": "deny",
                "user_message": f"Blocked read of credential/key file: {path}",
            }
        )
    )


def is_secret(file_path: str) -> bool:
    if not file_path:
        return False
    p = Path(file_path)
    name = p.name
    home = Path.home()
    if name == ".env" or name.startswith(".env.") or name.endswith(SUFFIXES):
        return True
    if name == "credentials" or name.startswith("credentials."):
        return True
    if "/.aws/" in file_path or "/.ssh/" in file_path or file_path.startswith("/dev/shm/"):
        return True
    try:
        return p.is_relative_to(home / ".aws") or p.is_relative_to(home / ".ssh")
    except (ValueError, OSError):
        return False


def main() -> None:
    raw = sys.stdin.read()
    data = json.loads(raw) if raw.strip() else {}
    file_path = data.get("file_path") or ""
    if is_secret(file_path):
        deny(file_path)
        return
    print(json.dumps({"permission": "allow"}))


if __name__ == "__main__":
    main()
