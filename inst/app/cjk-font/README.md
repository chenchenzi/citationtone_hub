# Optional font for Chinese characters in static plots

No font is bundled. Static plots (PNG panels and downloads) use an installed
Chinese font when their labels contain Chinese, Japanese or Korean characters
(see `cjk_family()` in `global.R`).

To make this work on a server without such a font, put a font file here, for
example a subset of Noto Sans SC Regular (SIL Open Font License), together
with its licence file. The app uses it before any installed font, with no code
change:

- `.otf`, `.ttf` or `.ttc`. It must contain both simplified and traditional
  characters: the app checks 阴阳发调陰陽發調平上去入声聲 and skips the
  file if any is missing.
- A second file with "bold" in its name, if present, is used for bold text.

Do not name this folder `fonts/`: systemfonts adds a `./fonts` folder by
itself.
