#!/usr/bin/env python3
"""Collect public usage figures for the About page's "In numbers" section.

Writes a small JSON file with
  * GitHub stars: the current count and the date each current stargazer
    starred the repository (people who unstar drop out of the history);
  * Zenodo: unique views and downloads across all versions of the archived
    release, as the record page shows them. Zenodo only reports totals, so
    one sample is kept per day and the history grows from the first run.

Run daily by .github/workflows/usage-stats.yaml, which commits the file to the
`usage-data` branch; the app reads it from there. Locally, it authenticates
with $GITHUB_TOKEN if set, otherwise through the `gh` CLI (listing stargazers
with dates needs authentication).

    python3 .github/scripts/usage_stats.py usage.json
"""

import datetime as dt
import json
import os
import shutil
import subprocess
import sys
import urllib.request

REPO = "chenchenzi/citationtone_hub"
ZENODO_CONCEPT = "22632508"   # concept record: all versions of the release
UA = "shinytone-usage-stats"


def get_json(url, headers=None):
    req = urllib.request.Request(url, headers={"User-Agent": UA, **(headers or {})})
    with urllib.request.urlopen(req, timeout=30) as r:   # follows redirects
        return json.load(r)


def github(path, accept="application/vnd.github+json"):
    """GET api.github.com/<path>, with $GITHUB_TOKEN or the gh CLI."""
    token = os.environ.get("GITHUB_TOKEN")
    if token:
        return get_json("https://api.github.com/" + path,
                        {"Accept": accept, "Authorization": "Bearer " + token,
                         "X-GitHub-Api-Version": "2022-11-28"})
    if shutil.which("gh"):
        out = subprocess.run(["gh", "api", "-H", "Accept: " + accept, path],
                             check=True, capture_output=True, text=True).stdout
        return json.loads(out)
    sys.exit("Set GITHUB_TOKEN or log in with the gh CLI: stargazer dates need authentication.")


def stars():
    count = github("repos/" + REPO)["stargazers_count"]
    dates, page = [], 1
    while True:
        batch = github(f"repos/{REPO}/stargazers?per_page=100&page={page}",
                       accept="application/vnd.github.star+json")
        dates += [s["starred_at"] for s in batch if s.get("starred_at")]
        if len(batch) < 100:
            break
        page += 1
    return {"count": count, "starred_at": sorted(dates)}


def zenodo():
    rec = get_json("https://zenodo.org/api/records/" + ZENODO_CONCEPT,
                   {"Accept": "application/json"})
    st = rec.get("stats") or {}
    return {"views": int(st["unique_views"]), "downloads": int(st["unique_downloads"]),
            "version": rec.get("metadata", {}).get("version")}


def main(path):
    old = {}
    if os.path.exists(path):
        with open(path, encoding="utf-8") as f:
            old = json.load(f)

    now = dt.datetime.now(dt.timezone.utc).replace(microsecond=0)
    today = now.date().isoformat()
    gh, zn = stars(), zenodo()   # any failure exits non-zero and leaves the old file

    history = [h for h in old.get("zenodo", {}).get("history", []) if h.get("date") != today]
    history.append({"date": today, "views": zn["views"], "downloads": zn["downloads"]})
    history.sort(key=lambda h: h["date"])

    out = {
        "updated": now.isoformat().replace("+00:00", "Z"),
        "github": {"repo": REPO, "stars": gh["count"], "starred_at": gh["starred_at"]},
        "zenodo": {"concept_doi": "10.5281/zenodo." + ZENODO_CONCEPT,
                   "latest_version": zn["version"],
                   "views": zn["views"], "downloads": zn["downloads"],
                   "history": history},
    }
    with open(path, "w", encoding="utf-8") as f:
        json.dump(out, f, indent=1, ensure_ascii=False)
        f.write("\n")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "usage.json")
