# Contributing to Shiden

## Quick start

- Fork the repo and create a branch from `main` named `feat/` or `fix/` or whatever you fell like.
- Run tests: `cargo test` and format: `cargo fmt` before pushing.
- Open a Pull Request and describe the problem, your change, and how you tested it.

## Development workflow

- Run `cargo fmt` and consider `cargo clippy` for linting.
- Add or update tests for bug fixes or new features. Prefer end-to-end tests that compile/execute sample programs when applicable.

## Tests & CI

- Unit & integration tests: `cargo test`.
- End-to-end compile tests: many tests in `src/build/mod.rs` create temporary projects, compile them and assert stdout.
- When debugging generated binaries, inspect with `objdump -d /tmp/<exe>`.

## Documentation

- Update or add docs in `docs/` for any feature, behavior, or developer guide (see `docs/build.md` for the compiler pipeline).
- Keep README and `docs/index.md` in sync for public-facing changes.

## Pull Request checklist

- [ ] Tests pass (`cargo test`).
- [ ] Code formatted (`cargo fmt`).
- [ ] Added/updated tests for new behavior.
- [ ] Updated docs when appropriate.
- [ ] Provide a clear PR description.

## Licensing & legal

By contributing you agree that your changes will be licensed under the repo's MIT license, see `LICENSE`.

## Need help?

Open an issue describing the problem or feature you'd like to work on, or start a draft PR and ask for review. We're happy to review and help iterate.

---