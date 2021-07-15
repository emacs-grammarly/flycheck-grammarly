[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/flycheck-grammarly-badge.svg)](https://melpa.org/#/flycheck-grammarly)
[![MELPA Stable](https://stable.melpa.org/packages/flycheck-grammarly-badge.svg)](https://stable.melpa.org/#/flycheck-grammarly)
[![CI](https://github.com/emacs-grammarly/flycheck-grammarly/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-grammarly/flycheck-grammarly/actions/workflows/test.yml)

<img align="right" src="./etc/logo.png" with="153" height="46">

# flycheck-grammarly
> Grammarly support for Flycheck.

<p align="center">
  <img src="./etc/screenshot.png" width="656" height="238"/>
<p>

## Usage

To enable this package, simply add loading to your config like the code below.

```el
(require 'flycheck-grammarly)
```

If you encounter the performance issue, try raise `flycheck-grammarly-check-time` higher.
The request will be send by this time everytime the buffer has changed.

```el
(setq flycheck-grammarly-check-time 0.8)
```

## Todo List

- [ ] Strip only text data, if other data like `# header` or `> quote` will return nothing.
- [ ] Usable but not fast enough, really depends on Grammarly's analyzer.

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
