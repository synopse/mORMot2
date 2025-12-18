# Future CLAUDE.md ↔ SAD Improvements

*Mejoras identificadas durante el alignment review de 2025-12-17*

---

## For CLAUDE.md Files

### 1. Fix "Last Updated" Dates
**Priority:** Low
**Effort:** ~30 min

Many CLAUDE.md files show `2025-10-10` which may be outdated. Consider updating to reflect actual last modification or removing dates entirely.

**Affected files:**
- `src/core/CLAUDE.md`
- `src/crypt/CLAUDE.md`
- `src/db/CLAUDE.md`
- `src/orm/CLAUDE.md`
- `src/rest/CLAUDE.md`
- `src/soa/CLAUDE.md`
- `src/net/CLAUDE.md`
- `src/app/CLAUDE.md`
- `src/lib/CLAUDE.md`
- `src/ui/CLAUDE.md`
- `src/misc/CLAUDE.md`

### 2. Add Layer Context Diagrams
**Priority:** Optional
**Effort:** ~2 hours

SAD Chapter 3 has excellent dependency layer diagrams. Could embed simplified versions in relevant CLAUDE.md files to show where each module fits.

**Candidates:**
- `src/core/CLAUDE.md` - Foundation layer diagram
- `src/orm/CLAUDE.md` - ORM stack diagram
- `src/rest/CLAUDE.md` - Client-Server architecture diagram
- `src/soa/CLAUDE.md` - SOA layer diagram

### 3. Expand Missing Patterns/Concepts
**Priority:** Medium
**Effort:** ~4 hours

From alignment reports, some CLAUDE.md files are missing patterns documented in SAD:

| CLAUDE.md | Missing from SAD |
|-----------|------------------|
| `src/crypt/` | Auth integration patterns (Ch 13) |
| `src/db/` | Virtual tables, FTS5 details (Ch 8) |
| `src/orm/` | Advanced filtering patterns (Ch 6) |
| `src/soa/` | Method-based services details (Ch 14) |
| `src/lib/` | Complete unit inventory |

**Reference:** See individual reports in `/mnt/w/mORMot2/DOCS/_working/alignment-reports/`

---

## For SAD Chapters

### 4. Add Library Linking Patterns to Chapter 3
**Priority:** Medium
**Effort:** ~1 hour

Chapter 3 covers architecture but could expand on:
- Static vs dynamic linking strategies per platform
- Conditional compilation patterns (`ZLIBSTATIC`, `USE_OPENSSL`, etc.)
- Cross-reference to `src/lib/CLAUDE.md`

### 5. Expand Authentication Examples in Chapter 13
**Priority:** Low
**Effort:** ~2 hours

Add more practical examples of:
- Custom authentication schemes
- Integration with external identity providers
- Session management patterns

### 6. Add UI/Report Module Documentation
**Priority:** Low
**Effort:** ~3 hours

Currently no SAD chapter covers `src/ui/`. Consider:
- PDF generation patterns
- Report engine usage
- VCL/LCL compatibility notes

---

## Alignment Report Archive

Full analysis reports preserved in:
```
/mnt/w/mORMot2/DOCS/_working/alignment-reports/
├── core-alignment.md
├── crypt-alignment.md
├── db-alignment.md
├── orm-alignment.md
├── rest-alignment.md
├── soa-alignment.md
├── net-alignment.md
├── app-alignment.md
├── ddd-alignment.md
├── script-alignment.md
├── lib-alignment.md
├── ui-alignment.md
├── misc-alignment.md
└── tools-alignment.md
```

---

*Generated: 2025-12-17*
*Source: PLAN-claude-md-sad-alignment.md*
