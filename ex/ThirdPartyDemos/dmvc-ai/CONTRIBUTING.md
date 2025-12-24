# Contributing to mORMot2 DMVC Samples

Thank you for your interest in contributing! This guide will help you get started.

## Ways to Contribute

### 1. Report Issues

- **Bug reports**: Compilation errors, runtime issues, incorrect behavior
- **Documentation gaps**: Missing explanations, unclear instructions
- **Suggestions**: New sample ideas, improvements to existing samples

### 2. Improve Documentation

- Fix typos and clarify explanations
- Add missing code comments
- Improve README files
- Add usage examples

### 3. Add New Samples

Port additional DMVC samples or create new ones demonstrating mORMot2 features.

### 4. Fix Bugs

Review open issues and submit fixes.

## Development Setup

### Prerequisites

- **Delphi 11+** or **Free Pascal 3.2+**
- **mORMot2** source code
- **Git**

### Getting Started

```bash
# Clone the repository
git clone https://github.com/[org]/mormot2-dmvc-samples.git
cd mormot2-dmvc-samples

# Ensure mORMot2 is in your library path
# Compile a sample to verify setup
```

## Sample Structure

Each sample should follow this structure:

```
XX-samplename/
├── src/
│   ├── api.interfaces.pas    # Service interface definitions
│   ├── api.impl.pas          # Service implementation
│   ├── entities.pas          # Domain entities (if needed)
│   └── server.pas            # HTTP server setup
├── www/                      # Static files (if needed)
├── SampleName.dpr            # Main program
├── SampleName.dproj          # Delphi project file
└── README.md                 # Sample documentation
```

## Coding Standards

### Pascal Code

- Use **mORMot2 idioms** (interface-based services, TDocVariant, etc.)
- Follow existing naming conventions in the codebase
- Add meaningful comments for non-obvious code
- Handle errors gracefully

### Documentation

- Each sample needs a README.md with:
  - What the sample demonstrates
  - How to build and run
  - Expected output
  - Link to original DMVC sample (if applicable)
  - Key mORMot2 concepts used

### Commit Messages

Use conventional commit format:

```
type(scope): description

feat(sample): add websocket authentication example
fix(12-middleware): correct CORS header handling
docs(README): clarify compilation instructions
```

Types: `feat`, `fix`, `docs`, `refactor`, `test`, `chore`

## Pull Request Process

1. **Fork** the repository
2. **Create a branch** for your changes
3. **Make your changes** following the standards above
4. **Test** your changes compile and run correctly
5. **Submit a PR** with a clear description

### PR Checklist

- [ ] Code compiles with Delphi 12 (0 errors, 0 warnings preferred)
- [ ] README.md is complete and accurate
- [ ] Sample follows the standard structure
- [ ] Commit messages are descriptive
- [ ] No unrelated changes included

## Adding a New Sample

1. Copy `_template/` to `XX-newsample/`
2. Update all file names and GUIDs
3. Implement the sample logic
4. Write comprehensive README.md
5. Test compilation and runtime
6. Submit PR

## Questions?

- Open an issue for questions about contributing
- Check existing samples for patterns and idioms
- See [mORMot2 documentation](https://synopse.info) for framework questions

## License

By contributing, you agree that your contributions will be licensed under the same terms as this project (see [LICENSE.md](LICENSE.md)).

---

Thank you for helping make these samples better!
