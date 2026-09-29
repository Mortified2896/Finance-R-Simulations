#!/usr/bin/env python3
"""Keep the deployment token outside Git and load it only for Wrangler."""
import getpass
import json
import os
from pathlib import Path
import re
import stat
import sys
import warnings

PROJECT = Path(__file__).resolve().parents[1]
DIRECTORY = Path.home() / ".config" / "finance-r-simulations"
CREDENTIAL = DIRECTORY / "cloudflare.json"


def fail(message):
    raise SystemExit(f"Cloudflare setup error: {message}")


def check_private(path, directory=False):
    info = path.lstat()
    expected = stat.S_ISDIR if directory else stat.S_ISREG
    if not expected(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
        fail(f"{path} must be owned by this user, not a symlink, and mode "
             f"{'700' if directory else '600'}.")


def validate(data):
    if not isinstance(data, dict):
        fail("Invalid credential format.")
    if not isinstance(data.get("api_token"), str) or not re.fullmatch(r"[A-Za-z0-9_-]+", data["api_token"]):
        fail("Missing or invalid API token.")
    for key in ("account_id", "zone_id"):
        if not isinstance(data.get(key), str) or not re.fullmatch(r"[a-fA-F0-9]{32}", data[key]):
            fail(f"{key} must be a 32-character Cloudflare ID.")
    return data


def install():
    if not sys.stdin.isatty():
        fail("Run install in your own interactive terminal; never pass a token as an argument.")
    DIRECTORY.mkdir(mode=0o700, parents=True, exist_ok=True)
    check_private(DIRECTORY, directory=True)
    if CREDENTIAL.exists() or CREDENTIAL.is_symlink():
        fail(f"Credential already exists at {CREDENTIAL}; no overwrite performed.")
    with warnings.catch_warnings():
        warnings.simplefilter("error", getpass.GetPassWarning)
        try:
            token = getpass.getpass("Scoped Cloudflare API token (hidden): ")
        except getpass.GetPassWarning:
            fail("A terminal with hidden input is required.")
    data = validate({"api_token": token.strip(),
                     "account_id": input("Cloudflare account ID: ").strip(),
                     "zone_id": input("Cloudflare zone ID: ").strip()})
    fd = os.open(CREDENTIAL, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600)
    with os.fdopen(fd, "w") as stream:
        json.dump(data, stream)
        stream.write("\n")
    print(f"Credential saved privately to {CREDENTIAL}. Authentication is not yet verified.")


def wrangler(arguments):
    if not arguments:
        fail("Supply a Wrangler command, for example: npm run cf -- d1 list")
    if arguments[0] in ("login", "logout", "auth"):
        fail("Use the scoped token installer, not interactive Wrangler authentication.")
    binary = PROJECT / "node_modules" / ".bin" / "wrangler"
    if not binary.exists():
        fail("Run npm ci in apps/article_lab_web first.")
    if not CREDENTIAL.exists():
        fail("No deployment credential installed. Run npm run cf:install-credential in your terminal.")
    check_private(DIRECTORY, directory=True)
    check_private(CREDENTIAL)
    try:
        data = validate(json.loads(CREDENTIAL.read_text()))
    except (ValueError, UnicodeError):
        fail("Credential JSON is invalid; its contents have not been printed.")
    environment = {key: value for key, value in os.environ.items()
                   if not key.startswith(("CLOUDFLARE_", "CF_", "WRANGLER_"))}
    environment.update(CLOUDFLARE_API_TOKEN=data["api_token"],
                       CLOUDFLARE_ACCOUNT_ID=data["account_id"],
                       CLOUDFLARE_ZONE_ID=data["zone_id"],
                       WRANGLER_SEND_METRICS="false", WRANGLER_LOG="info")
    os.umask(0o077)
    os.execve(binary, [str(binary), *arguments], environment)


if __name__ == "__main__":
    try:
        if sys.argv[1:] == ["install"]:
            install()
        elif sys.argv[1:2] == ["wrangler"]:
            wrangler(sys.argv[2:])
        else:
            fail("Usage: cloudflare.py install | wrangler <arguments>")
    except (OSError, EOFError):
        fail("Could not access the terminal, credential file, or Wrangler executable.")
