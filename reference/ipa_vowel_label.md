# Does an interval label denote a vowel (IPA)?

Vectorised heuristic for picking vowel intervals out of a segmental
TextGrid tier without knowing the language. A label counts as a vowel
when, after stripping length marks, stress marks, tone digits/letters,
superscripts, ties, spaces, and combining diacritics, every remaining
character is an IPA vowel letter — so long vowels (`aː`), nasalised
vowels (`ã`), and di-/triphthongs (`ai`, `iau`) all match, while
anything containing a consonant letter (`ang`, `pa`, `n`) does not.

Di- and triphthongs need no special handling: the test applies to
*every* character of the label, so a label matches when all of its
letters are vowels – `ai`, `au`, `iau`, `uai`, `ɔi`, `aːi` and `ai̯` all
pass.

Offglides written as consonant letters are accepted too, since many
traditions spell diphthongs that way: `j`, `w` and `ɥ` count inside a
nucleus that *also* contains a vowel letter, so `aj`, `aw`, `ja`, `jaw`
and `waj` match, while a bare `j` or `w` onset does not. A vowel mixed
with any other consonant (`ang`, `an`, `pa`) never matches.

Syllabic nasals count as vowel-equivalent nuclei: a label whose letters
are all nasals and which carries a syllabicity mark (`m̩`, `n̩`, `ŋ̩`)
matches, since these are tone-bearing units in e.g. Cantonese (唔, 五).
The same nasals *without* the mark (`m`, `n`, `ŋ`) do not.

## Usage

``` r
ipa_vowel_label(x)
```

## Arguments

- x:

  Character vector of interval labels (`NA` and empty labels return
  `FALSE`).

## Value

Logical vector, same length as `x`.

## Details

Recognised base letters: the IPA vowel letters
(`a e i o u y æ ɐ ɑ ɒ ə ɘ ɵ ɛ œ ɜ ɞ ɤ ɪ ɨ ɔ ø ʉ ʊ ʌ ɯ ʏ ɶ ɚ ɝ` and the
near-close central `ᵻ ᵿ`), their ASCII upper-case counterparts
`A E I O U Y`, `@` (schwa in SAMPA-style labels), and precomposed
accented Latin vowels, including the pinyin tone-marked forms
(`ã é ü ā á ǎ à ū ǔ ǖ ǘ ǚ ǜ` ...) in either Unicode normalisation.
Labels in other schemes (whole pinyin finals such as `ang`, X-SAMPA
consonant-bearing rhymes) should be selected with explicit labels
instead — see
[`filter_interval_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/filter_interval_rows.md).

## See also

[`filter_interval_rows()`](https://chenchenzi.github.io/citationtone_hub/reference/filter_interval_rows.md)
for subsetting f0 frames by interval.
