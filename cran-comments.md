## Test environments
* local Windows R 3.2.2 x86

## R CMD check results

There were 3 NOTE and 1 WARNING, most of them related to not having installed some tools (but they have passed CRAN builds before). I have already fixed the problems that Uwe Ligges reported by email in the submission from 2015-08-19.

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Juan Manuel Truppia <jmtruppia@gmail.com>'
License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2014
  COPYRIGHT HOLDER: Juan Manuel Truppia
* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped
* checking top-level files ... NOTE
File README.md cannot be checked without 'pandoc' being installed.
* checking for unstated dependencies in examples ... OK
 WARNING
'qpdf' is needed for checks on size reduction of PDFs
