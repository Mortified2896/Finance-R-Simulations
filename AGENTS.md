# Agent Instructions

## Stable and Design Variants

* Stable/default local app: `http://127.0.0.1:3840/`
* Experimental Design v2: `http://127.0.0.1:3844/`

Both are variants of the same application. Design v2 is used for visual experimentation and is not an isolated mockup.

Work intended for the stable app is not complete until it has been implemented and verified on port `3840`.

Do not treat a temporary port, copied database, audit instance, or other preview as stable-app completion.

## Prefer Current Functionality Over Legacy Compatibility

Prefer updating current functionality over preserving obsolete generated data, legacy workflow state, old batches, or outdated local database structures.

Do not add substantial compatibility or migration complexity unless explicitly requested.

Obsolete runtime data may be regenerated when it is confirmed to be local-only and reproducible. Before deleting material data, identify the exact target, explain the impact, and obtain user approval unless deletion was explicitly requested.

## Make AI Generation Traceable

For every AI-generated output, users must be able to understand what produced it.

Record or expose the substantive inputs, instructions, supporting context, model identifier, and relevant generation settings.

Do not create opaque generation workflows that show only the final output.

Never expose credentials, secrets, private identifiers, or unrestricted raw context merely for traceability.

## Fail Loud

User-triggered failures must be clear, prominent, and persistent.

Do not silently continue, hide failures in muted status text, or display success after partial completion. Every new user-triggered workflow must have an explicit visible failure path.

## Keep Runtime Data Local

Do not commit databases, downloaded media, browser profiles, raw API outputs, generated queues, model outputs, caches, credentials, or temporary artifacts.

Runtime and scratch data must remain in ignored locations.
