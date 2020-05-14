# rivet-mode - Minor mode for editing Apache Rivet

*Author:* Jade Michael Thornton<br>
*Version:* 4.0.2<br>

[![MELPA: rivet-mode](file:https://melpa.org/packages/rivet-mode-badge.svg)](https://melpa.org/#/rivet-mode) [![ISC License](file:https://img.shields.io/badge/license-ISC-green.svg)](./LICENSE) [![](file:https://img.shields.io/github/languages/code-size/thornjad/rivet-mode.svg)](https://gitlab.com/thornjad/rivet-mode) [![](file:https://img.shields.io/github/v/tag/thornjad/rivet-mode.svg?label=version&color=yellowgreen)](https://gitlab.com/thornjad/rivet-mode/-/tags)

Rivet mode is a minor mode for editing Apache Rivet files. It automatically
detects whether TCL or HTML is currently being edited and uses the major
modes tcl-mode and web-mode, respectively.

By default, `rivet-mode` requires `tcl` (built-in) and `web-mode`. To use another mode,
customize `rivet-mode-host-mode` and `rivet-mode-inner-mode` to suit.

The variable =rivet-mode-host-mode= determines the "host" major mode, which
is web-mode by default.

The variable =rivet-mode-inner-mode= determines the "inner" major mode, which
is the built-in tcl-mode by default.

The variable =rivet-mode-delimiters= defines the left and right delimiters
which demark the bounds of the "inner" major mode (TCL). These are "<?" and
"?>" by default. Note that the "<?=" delimiter, which marks the start of an
expression, still begins with "<?" and so will be caught.


---
Converted from `rivet-mode.el` by [*el2md*](https://gitlab.com/thornjad/el2md).
