# lilypond-bend-spanner

This is a copy of the lilypond-bend-spanner repository available at
https://pagure.io/lilypond-bend-spanner, but updated to work with LilyPond
2.22.0. (LilyPond commit [`06ba7f0823a14da773391ccf3c10244a54e812d4`](https://git.savannah.gnu.org/cgit/lilypond.git/commit/?id=06ba7f0823a14da773391ccf3c10244a54e812d4)
eliminated the requirement of quoting stencil expressions, and the original
lilypond-bend-spanner
[contains](https://pagure.io/lilypond-bend-spanner/blob/918690efca06d4d1fb49b2e2c955195a635540cb/f/bend-stencils.ly#_39)
such an expression. This version
[unquotes](https://github.com/nwhetsell/lilypond-bend-spanner/commit/ef2ebcb0a812d8395046aab105f2cdc5668d64f4#diff-16a1e59ba30595ad750c2b3b3186a80fcd87ad6b84ee64c87823e2770cdab8b9)
that expression.)
