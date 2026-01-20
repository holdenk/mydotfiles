# Emacs Configuration Documentation

This document explains the key bindings and features of this Emacs configuration.

## Table of Contents

- [Overview](#overview)
- [Installation](#installation)
- [Global Key Bindings](#global-key-bindings)
- [Language-Specific Key Bindings](#language-specific-key-bindings)
  - [Scala](#scala)
  - [Java](#java)
  - [Clojure](#clojure)
- [Git Integration (Magit)](#git-integration-magit)
- [Auto-Completion (Company Mode)](#auto-completion-company-mode)
- [GitHub Copilot](#github-copilot)
- [File Navigation](#file-navigation)
- [LSP Features](#lsp-features)
- [Platform-Specific Notes](#platform-specific-notes)
- [Troubleshooting](#troubleshooting)

## Overview

This Emacs configuration is optimized for:
- **Scala development** with Metals LSP server
- **Java development** with Eclipse JDT Language Server
- **Python development** with Pyright
- **Git workflows** with Magit
- **AI-assisted coding** with GitHub Copilot
- **Multi-language support** (Rust, Go, TypeScript, Clojure, etc.)

**Tested with:** Emacs 27.1+ (including Emacs 31.0+)

**Platforms:** Linux (primary), macOS (tested)

## Installation

### Quick Setup

1. **Copy configuration file:**
   ```bash
   cp emacs ~/.emacs
   # OR
   cp emacs ~/.emacs.d/init.el
   ```

2. **Start Emacs:**
   ```bash
   emacs
   ```
   Packages will be installed automatically on first launch.

3. **Install language servers (optional but recommended):**
   
   - **Scala (Metals):** Follow [Metals installation guide](https://scalameta.org/metals/docs/editors/emacs/)
   - **Java:** Automatically installed via lsp-java on first use
   - **Python (Pyright):** `npm install -g pyright`
   - **Rust:** `rustup component add rust-analyzer`

### macOS-Specific Setup

On macOS, this configuration automatically:
- Sets Command key as Meta
- Sets Option key as Super
- Adds Homebrew paths (`/usr/local/bin`, `/opt/homebrew/bin`) to exec-path

## Global Key Bindings

### Essential Bindings

| Key Binding | Command | Description |
|------------|---------|-------------|
| `C-x g` | `magit-status` | Open Magit (Git interface) |
| `C-x t` | `ftf-find-file` | Fast file finder |
| `C-x p` | `project-find-file` | Find file in current project |
| `C-g` | `keyboard-quit` | Cancel current command |
| `C-x C-s` | `save-buffer` | Save current file |
| `C-x C-c` | `save-buffers-kill-terminal` | Exit Emacs |

### Navigation

| Key Binding | Command | Description |
|------------|---------|-------------|
| `C-n` | `next-line` | Move down one line |
| `C-p` | `previous-line` | Move up one line |
| `C-f` | `forward-char` | Move forward one character |
| `C-b` | `backward-char` | Move backward one character |
| `C-a` | `beginning-of-line` | Move to beginning of line |
| `C-e` | `end-of-line` | Move to end of line |
| `M-f` | `forward-word` | Move forward one word |
| `M-b` | `backward-word` | Move backward one word |
| `M-<` | `beginning-of-buffer` | Move to beginning of file |
| `M->` | `end-of-buffer` | Move to end of file |

### Editing

| Key Binding | Command | Description |
|------------|---------|-------------|
| `C-d` | `delete-char` | Delete character under cursor |
| `M-d` | Context-dependent | Delete word OR find definition (see language sections) |
| `C-k` | `kill-line` | Delete from cursor to end of line |
| `C-y` | `yank` | Paste (yank) last killed text |
| `M-y` | `yank-pop` | Cycle through kill ring |
| `C-/` or `C-_` | `undo` | Undo last change |
| `C-space` | `set-mark-command` | Start selection |

## Language-Specific Key Bindings

### Scala

Scala mode includes integration with SBT and Metals LSP server.

| Key Binding | Command | Description |
|------------|---------|-------------|
| `M-d` | `sbt-find-definitions` / `lsp-find-definition` | Find definition (grep or LSP) |
| `M-.` | `lsp-find-definition` | Navigate to definition (LSP) |
| `C-x '` | `sbt-run-previous-command` | Re-run last SBT command |
| `RET` | `newline-and-indent` | New line with auto-indent |

**Features:**
- Auto-completion via Metals and Company mode
- Flyspell for spell checking in comments
- Automatic whitespace cleanup on save
- JavaDoc-style indentation
- Multi-line comment asterisk insertion

### Java

Java mode includes Eclipse JDT Language Server support.

| Key Binding | Command | Description |
|------------|---------|-------------|
| `M-.` | `lsp-find-definition` | Navigate to definition |
| `M-d` | `lsp-find-definition` | Find definition |
| `RET` | `newline-and-indent` | New line with auto-indent |

**Indentation:**
- 2 spaces (not tabs)
- Auto-indent on newline

### Clojure

Clojure support via CIDER (Clojure Interactive Development Environment).

**Features:**
- REPL integration
- Eldoc for function signatures
- Subword mode for better navigation
- Company mode completion

**Note:** Use standard CIDER key bindings. See [CIDER documentation](https://docs.cider.mx/cider/usage/key_bindings.html) for details.

### Python

Python support via Pyright LSP server.

**Features:**
- Type checking via Pyright
- Auto-completion
- Jump to definition

### Other Languages

- **Rust:** rust-mode with LSP support
- **Go:** go-mode
- **TypeScript:** typescript-mode
- **YAML:** yaml-mode with syntax highlighting
- **Markdown:** markdown-mode
- **Dockerfile:** dockerfile-mode
- **AsciiDoc:** adoc-mode with spell checking

## Git Integration (Magit)

Magit is a powerful Git interface for Emacs.

### Starting Magit

- `C-x g` - Open Magit status buffer

### Common Magit Commands (in status buffer)

| Key | Command | Description |
|-----|---------|-------------|
| `s` | Stage | Stage file or hunk under cursor |
| `u` | Unstage | Unstage file or hunk |
| `c c` | Commit | Create commit |
| `P p` | Push | Push to remote |
| `F p` | Pull | Pull from remote |
| `b b` | Branch | Switch branches |
| `l l` | Log | View commit log |
| `d d` | Diff | View diff |
| `q` | Quit | Close Magit buffer |
| `?` | Help | Show all available commands |

**Tip:** Press `?` in any Magit buffer to see all available commands.

## Auto-Completion (Company Mode)

Company mode provides auto-completion across all programming modes.

### Company Mode Key Bindings

| Key Binding | Command | Description |
|------------|---------|-------------|
| `C-<down>` | `company-select-next` | Select next completion |
| `C-<up>` | `company-select-previous` | Select previous completion |
| `C-<return>` | `company-complete-selection` | Accept selected completion |
| `ESC` | Cancel | Cancel completion |

**Note:** Standard TAB, Return, and arrow keys are disabled for completion to avoid conflicts. Use the bindings above.

## GitHub Copilot

GitHub Copilot provides AI-powered code suggestions.

### Copilot Key Bindings

| Key Binding | Command | Description |
|------------|---------|-------------|
| `M-<next>` (M-PageDown) | `copilot-next-completion` | Show next suggestion |
| `M-<prior>` (M-PageUp) | `copilot-previous-completion` | Show previous suggestion |
| `M-<right>` | `copilot-accept-completion-by-word` | Accept next word |
| `M-<down>` | `copilot-accept-completion-by-line` | Accept next line |
| `ESC` | Dismiss | Dismiss current suggestion |

**Notes:**
- Copilot is automatically enabled in all programming modes
- Requires GitHub Copilot subscription and authentication
- First-time setup: You'll need to run `M-x copilot-login` and follow the authentication flow

## File Navigation

### Finding Files

| Key Binding | Command | Description |
|------------|---------|-------------|
| `C-x C-f` | `find-file` | Open file (with path completion) |
| `C-x t` | `ftf-find-file` | Fast fuzzy file finder |
| `C-x p` | `project-find-file` | Find file in current project |
| `C-x b` | `switch-to-buffer` | Switch to open buffer |

### Find-Things-Fast Configuration

- Searches all file types
- Ignores: `.git`, `target`, `connector` directories
- Fast fuzzy matching

## LSP Features

LSP (Language Server Protocol) provides intelligent code features.

### LSP Commands

| Command | Description |
|---------|-------------|
| `M-x lsp` | Start LSP server manually |
| `M-x lsp-describe-thing-at-point` | Show documentation |
| `M-x lsp-rename` | Rename symbol |
| `M-x lsp-find-references` | Find all references |
| `M-x lsp-format-buffer` | Format current buffer |

### LSP-UI

LSP-UI provides visual enhancements:
- Inline documentation
- Sideline information
- Peek definitions

### Supported Languages

- **Scala:** Metals (requires separate installation)
- **Java:** Eclipse JDT (auto-installed)
- **Python:** Pyright (requires `npm install -g pyright`)
- **Rust:** rust-analyzer (via rustup)
- **XML:** XML language server

## Platform-Specific Notes

### macOS

**Automatic Configuration:**
- Command (⌘) key mapped to Meta
- Option (⌥) key mapped to Super
- Homebrew paths added to exec-path

**Font Smoothing:**
- Enabled for better display on Retina screens

**Path Configuration:**
- Both Intel (`/usr/local/bin`) and Apple Silicon (`/opt/homebrew/bin`) Homebrew paths supported

### Linux

- Default configuration optimized for Linux
- No special platform-specific settings needed

## Troubleshooting

### Packages Not Installing

**Problem:** Packages fail to install on first launch.

**Solutions:**
1. Check internet connection
2. Try `M-x package-refresh-contents`
3. Try `M-x package-install <package-name>` manually

### LSP Not Working

**Problem:** LSP features not available for a language.

**Solutions:**
1. Ensure language server is installed:
   - Scala: Install Metals
   - Python: `npm install -g pyright`
   - Rust: `rustup component add rust-analyzer`
2. Try `M-x lsp` manually in the buffer
3. Check `*lsp-log*` buffer for errors

### Copilot Not Working

**Problem:** GitHub Copilot not providing suggestions.

**Solutions:**
1. Run `M-x copilot-login` to authenticate
2. Check if you have an active GitHub Copilot subscription
3. Ensure `copilot-mode` is enabled: `M-x copilot-mode`
4. Check `*copilot stderr*` buffer for errors

### Slow Performance

**Problem:** Emacs feels sluggish with large projects.

**Solutions:**
1. This config already includes performance optimizations:
   - `gc-cons-threshold`: 100MB
   - `read-process-output-max`: 1MB
2. For very large projects, consider:
   - Reducing `lsp-file-watch-threshold` (currently 200,000)
   - Disabling LSP for some modes: `M-x lsp-disconnect`
3. Check background processes: `M-x list-processes`

### macOS-Specific Issues

**Problem:** Command key not working as Meta.

**Solution:** This config sets it automatically, but if needed:
```elisp
(setq mac-command-modifier 'meta)
```

**Problem:** Can't find language servers installed via Homebrew.

**Solution:** This config adds Homebrew paths automatically. If issues persist:
```elisp
(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
```

### Font Size Issues

**Problem:** Font too large or too small.

**Solution:** Edit the height value in your emacs file (around line 60):
```elisp
;; Default is 230 for 4K displays
(set-face-attribute 'default nil :height 230)

;; Try these values:
;; 120 for standard HD displays
;; 160 for 2K displays
;; 230 for 4K displays
```

## Additional Resources

- [Emacs Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html)
- [Magit User Manual](https://magit.vc/manual/magit/)
- [LSP Mode Documentation](https://emacs-lsp.github.io/lsp-mode/)
- [Metals Emacs Guide](https://scalameta.org/metals/docs/editors/emacs/)
- [CIDER Documentation](https://docs.cider.mx/cider/)
- [Company Mode Manual](https://company-mode.github.io/)

## Quick Reference Card

### Most Common Commands

```
Finding Things:
  C-x C-f     Open file
  C-x t       Fast find
  C-x p       Project find
  M-.         Jump to definition

Git:
  C-x g       Magit status

Editing:
  C-space     Start selection
  C-w         Cut
  M-w         Copy
  C-y         Paste
  C-k         Kill line

Completion:
  C-<down>    Next completion
  C-<return>  Accept completion

Copilot:
  M-<down>    Accept line
  M-<right>   Accept word

Help:
  C-h k       Describe key
  C-h f       Describe function
  C-h v       Describe variable
  C-h m       Describe mode
```

---

**Need Help?** Press `C-h ?` in Emacs to see all help options, or `C-h k` followed by any key binding to see what it does.
