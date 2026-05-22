## Resubmission

Dear Benjamin Altmann,

Thank you for the review. I have addressed all the points raised (version 1.1.1):

* **Title/Description:** Removed the redundant "in R" from the title, and put
  all software/format/package names in single quotes ('GDX', 'GAMS',
  'gamstransfer').
* **References:** There is no publication describing the methods; the package
  is an I/O utility. I added the 'GDX' format documentation as an auto-linked
  URL in the Description for context.
* **\value tags:** Added \value to all exported functions (`gdx`, `extract`,
  `extract.gdx`, `all_items`, `batch_extract`, `write.gdx`, `write2.gdx`),
  describing the output class/structure, or noting the side effect for
  functions called for their effect.
* **\dontrun:** Replaced all \dontrun{} examples with self-contained,
  executable examples (writing to tempfile() and reading back); each runs in
  well under 5 seconds.

Thank you for your time.

Best regards,
Laurent Drouet

## Test environments

* local: R CMD check --as-cran

## R CMD check results

0 errors | 0 warnings | 1 note

* The note is the standard "New submission" maintainer note.
