# pinyin-convert.el

This is a tool to convert pinyin written with diacritics to pinyin written with tone numbers, and vice versa.

For example, `tóng` can be written as `tong2`.

You can use it by highlighting a region containing pinyin and running either `pinyin-convert-to-tone-number` or `pinyin-convert-to-tone-mark`.

When converting to tone mark pinyin, the character `v` is converted to the character `ü`.

## TODO

- convert tone number pinyin that use the `ü` character
- convert tone number pinyin that use `:u` or `u:`
- write tests that make sure we don't convert syllables that aren't in Standard Chinese,  like `yam` or `bua`

## Some details

`syllables.txt` contains a list of [all Hanyu Pinyin syllables](https://en.wikipedia.org/wiki/Pinyin_table) that are used in Standard Chinese. Each of these syllables needs to be combined with a tone to create an actual pinyin syllable. Some of them can be combined with all five tones, but most can only be combined with a subset of the five tones.

For now at least, `pinyin-convert` assumes all combinations of tones and syllables in `syllables.txt` are possible, and it will convert them all.

Additionally, it assumes that an `r` can be appended to any pinyin syllable and combined with any tone to create an "erhua" syllable.



