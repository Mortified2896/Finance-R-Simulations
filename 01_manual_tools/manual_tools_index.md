# Manual Tools

Double-clickable launchers and browser helpers are grouped by workflow:

- `analysis/`: schema setup, validation, title/subtitle scoring checks, and terminal rating.
- `images/`: thumbnail scoring checks and Medium image download utilities. Longer interactive workflows are implemented in `scripts/manual_tools/` and called by thin `.command` wrappers.
- `import/`: manual stats importers, bookmarklets, and import notes. The file-drop importer loop lives in `scripts/manual_tools/` and is called by a thin `.command` wrapper.
- `rating/`: local browser-based human rating apps, including the opt-in `rate_medium_previews_design_v2.command` launcher for UI design experiments.
- `watchers/`: Medium tag/page watcher launchers and snippets. The attached-Chrome workflow lives in `scripts/manual_tools/` and is called by a thin `.command` wrapper.
- `reference/`: small reference notes used by manual tools.
