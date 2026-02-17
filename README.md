# XMonad Configuration

Personal XMonad configuration with BSP tiling, multi-monitor support via IndependentScreens, and [eww](https://github.com/elkowar/eww) status bars.

**Mod key:** `Alt`

## Requirements

- XMonad 0.17+
- [xmonad-contrib](https://github.com/xmonad/xmonad-contrib)
- [eww](https://github.com/elkowar/eww) (status bar)
- [alacritty](https://github.com/alacritty/alacritty) (terminal)
- [rofi](https://github.com/davatorium/rofi) (program launcher)
- [firefox](https://www.mozilla.org/firefox/) (browser)
- [betterlockscreen](https://github.com/betterlockscreen/betterlockscreen) (lock screen)

## Structure

```
~/.config/xmonad/
├── xmonad.hs       # Full configuration (single file)
└── hie.yaml        # HLS project config
```

## Workspaces

Four workspaces per screen, interleaved for multi-monitor support:

| Index | Name | Purpose |
|-------|------|---------|
| 1 | `term` | Terminals |
| 2 | `code` | Editor / IDE |
| 3 | `browser` | Web browser |
| 4 | `comm` | Communication |

Each monitor gets its own independent set of these workspaces via `IndependentScreens`.

## Layouts

Cycle layouts with `Alt+[` or `Alt+]`.

| Layout | Spacing | Description |
|--------|---------|-------------|
| BSP | 40px | Binary space partition tiling |
| Tall | 30px | Master/stack (vertical split) |
| Mirror Tall | 30px | Master/stack (horizontal split) |
| Monocle | 40px | Single window with gaps |
| Fullscreen | 0px | True fullscreen, no struts |

## Keybindings

### General

| Key | Action |
|-----|--------|
| `Alt+Ctrl+r` | Recompile and restart XMonad |
| `Alt+Shift+e` | Quit XMonad |
| `Alt+Return` | Open terminal (alacritty) |
| `Alt+Space` | Open program launcher (rofi) |
| `Alt+b` | Open browser (firefox) |
| `Super+l` | Lock screen |

### Window Management

| Key | Action |
|-----|--------|
| `Alt+w` | Close focused window |
| `Alt+Shift+q` | Close focused window |
| `Alt+Ctrl+w` | Close all windows on workspace |

### Layout Management

| Key | Action |
|-----|--------|
| `Alt+[` | Cycle to next layout |
| `Alt+]` | Cycle to next layout |
| `Alt+Ctrl+b` | Toggle status bar struts |
| `Alt+Ctrl+,` | Shrink master area |
| `Alt+Ctrl+.` | Expand master area |
| `Alt+Ctrl+Shift+,` | Remove window from master |
| `Alt+Ctrl+Shift+.` | Add window to master |

### Workspace Navigation

| Key | Action |
|-----|--------|
| `Alt+1` | Switch to workspace 1 (term) |
| `Alt+2` | Switch to workspace 2 (code) |
| `Alt+3` | Switch to workspace 3 (browser) |
| `Alt+4` | Switch to workspace 4 (comm) |
| `Alt+Shift+1` | Move window to workspace 1 |
| `Alt+Shift+2` | Move window to workspace 2 |
| `Alt+Shift+3` | Move window to workspace 3 |
| `Alt+Shift+4` | Move window to workspace 4 |
| `Alt+Tab` | Next workspace |
| `Alt+Shift+Tab` | Previous workspace |

### Screen Navigation

| Key | Action |
|-----|--------|
| `Alt+,` | Focus previous screen |
| `Alt+.` | Focus next screen |
| `Alt+Shift+,` | Move window to previous screen and follow |
| `Alt+Shift+.` | Move window to next screen and follow |

### Layout-Aware Navigation

Navigation keys adapt to the active layout. The table below shows what each key does per layout.

#### BSP Layout

| Key | Action |
|-----|--------|
| `Alt+h/j/k/l` | Navigate left/down/up/right |
| `Alt+Shift+h/j/k/l` | Swap window left/down/up/right |
| `Alt+Ctrl+h/j/k/l` | Resize (shrink from right / expand down / shrink from down / expand right) |
| `Alt+s` | Swap subtrees |
| `Alt+r` | Rotate split |

#### Tall Layout

| Key | Action |
|-----|--------|
| `Alt+j/k` | Focus next/previous window |
| `Alt+Shift+j/k` | Swap next/previous window |
| `Alt+h/l` | Shrink/expand master area |

#### Mirror Tall Layout

| Key | Action |
|-----|--------|
| `Alt+h/l` | Focus previous/next window |
| `Alt+Shift+h/l` | Swap previous/next window |
| `Alt+j/k` | Shrink/expand master area |

#### Monocle / Fullscreen Layout

| Key | Action |
|-----|--------|
| `Alt+j/k` | Focus next/previous window |

## Status Bar

Uses eww with `StatusBarPropTo` for EWMH-compatible workspace/layout reporting. Each screen gets its own bar instance (`top-bar-0`, `top-bar-1`, ...) reading from `_XMONAD_LOG_<N>` X properties.

The PP format outputs workspace entries as `state:name:index` separated by `|`, with layout appended after `|||`.

## Options

| Option | Value | Description |
|--------|-------|-------------|
| `modMask` | `Alt` | Modifier key |
| `terminal` | `alacritty` | Default terminal |
| `focusFollowsMouse` | `true` | Focus follows mouse pointer |
| `borderWidth` | `0` | No window borders |
| `normalBorderColor` | `#71376A` | Unfocused border color |
| `focusedBorderColor` | `#71376A` | Focused border color |

## XMonad-Contrib Modules

| Module | Purpose |
|--------|---------|
| `Actions.CopyWindow` | `kill1` for closing windows |
| `Actions.CycleWS` | Workspace and screen cycling |
| `Actions.DynamicWorkspaces` | Nth workspace switching |
| `Actions.WithAll` | `killAll` for closing all windows |
| `Hooks.EwmhDesktops` | EWMH compliance |
| `Hooks.ManageDocks` | Dock/bar awareness |
| `Hooks.StatusBar` | Status bar integration |
| `Layout.BinarySpacePartition` | BSP tiling layout |
| `Layout.IndependentScreens` | Per-screen workspaces |
| `Layout.Renamed` | Custom layout names |
| `Layout.Spacing` | Window gaps |
| `Layout.WindowNavigation` | Directional window navigation |
| `Util.EZConfig` | Emacs-style keybinding syntax |
