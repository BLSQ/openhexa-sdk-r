#!/usr/bin/env python
"""Validate the package's GraphQL operations (inst/graphql/*.graphql) against the OpenHEXA schema.

The schema is fetched by introspection from a live server (production by default), or read from a local
SDL file. Exits with code 1 if any operation is invalid, e.g. because a field it selects was renamed or removed.

Usage:
    python tools/validate_queries.py                       # against https://api.openhexa.org
    python tools/validate_queries.py --url https://api.demo.openhexa.org
    python tools/validate_queries.py --schema path/to/schema.graphql
    python tools/validate_queries.py --save-schema tools/schema.graphql   # also save the schema, for the IDE
"""

import argparse
import os
import sys
from pathlib import Path

import requests
from graphql import build_client_schema, build_schema, get_introspection_query, parse, print_schema, validate

QUERIES_DIR = Path(__file__).resolve().parent.parent / "inst" / "graphql"
DEFAULT_URL = "https://api.openhexa.org"
GITHUB_ACTIONS = os.getenv("GITHUB_ACTIONS") == "true"


def fetch_schema(server_url: str):
    """Fetch the GraphQL schema from a server via introspection."""
    response = requests.post(
        f"{server_url.rstrip('/')}/graphql/",
        json={"query": get_introspection_query(input_value_deprecation=True)},
        timeout=30,
    )
    response.raise_for_status()
    body = response.json()
    if "errors" in body:
        raise RuntimeError(f"Introspection query returned errors: {body['errors']}")
    return build_client_schema(body["data"])


def main():
    """Validate every operation file and report errors."""
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    source = parser.add_mutually_exclusive_group()
    source.add_argument("--url", default=DEFAULT_URL, help="OpenHEXA server to introspect")
    source.add_argument("--schema", type=Path, help="Local schema file (SDL) instead of introspection")
    parser.add_argument("--save-schema", type=Path, help="Save the schema (SDL) to this file")
    args = parser.parse_args()

    if args.schema:
        schema = build_schema(args.schema.read_text(), assume_valid_sdl=True)
        origin = str(args.schema)
    else:
        schema = fetch_schema(args.url)
        origin = args.url
    if args.save_schema:
        args.save_schema.write_text(print_schema(schema) + "\n")
        print(f"Saved the schema of {origin} to {args.save_schema}.")

    files = sorted(QUERIES_DIR.glob("*.graphql"))
    failures = 0
    for path in files:
        document = parse(path.read_text())
        operations = [d for d in document.definitions if d.kind == "operation_definition"]
        if len(operations) != 1 or operations[0].name is None or operations[0].name.value != path.stem:
            print(f"✗ {path.name}: must contain exactly one operation, named {path.stem}")
            failures += 1
            continue

        errors = validate(schema, document)
        if errors:
            failures += 1
            print(f"✗ {path.name}")
            for error in errors:
                print(f"    {error.message}")
                if GITHUB_ACTIONS:
                    print(f"::error file=inst/graphql/{path.name},title=Invalid GraphQL operation::{error.message}")

    if failures:
        print(f"\n{failures} of {len(files)} operation(s) are invalid against {origin}.")
        sys.exit(1)
    print(f"✅ All {len(files)} operations are valid against {origin}.")


if __name__ == "__main__":
    main()
