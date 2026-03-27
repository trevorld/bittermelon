## Test environments

* local (linux, R 4.5.3)
* win-builder (windows, R devel)
* github actions (linux, R devel)
* github actions (linux, R release)
* github actions (linux, R oldrel)

## R CMD check --as-cran results

* 1 NOTE

Found the following (possibly) invalid URLs:
  URL: https://www.kreativekorp.com/ucsur/
    From: inst/doc/bittermelon.html
          README.md
    Status: 406
    Message: Not Acceptable

I manually verified that https://www.kreativekorp.com/ucsur/ is a valid URL
