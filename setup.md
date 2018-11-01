
This package was developed with two goals in mind.

1. Document and maintain scripts for an ongoing project
2. Practice using continuous integration and unit testing in the context of an R package


### Create an R package locally

Basic R package setup per Hilary Parker's tutorial [here](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/).

In my R session, 
```
library("roxygen2")
library("devtools")
create("metahostlab")
```

the, I continued following the tutorial for documentation and initial installation of the package. 

### Adding unit tests

Per [this tutorial](http://r-pkgs.had.co.nz/tests.html), the command below creates a directory **/tests/** containing file **testthat.R**, which will run all tests when you build the package.

```
devtools::use_testthat()
```

However, this doesn't generate any tests. In order to have actual tests for the above generated script to run on build, you need to add individual test files to the **/testthat** directory.

Each test script should begin with the name "test" and generally follow the naming convention "test-XX.R". For reference, [this](https://github.com/tidyverse/stringr/blob/master/tests/testthat/test-case.R) is a good example of a package with unit tests.

In order to run the tests, I can manually call the unit tests via the following command:

```
setwd("metahostlab")
devtools::test()
```

Which results in the following output the the R console:
```
Loading metahostlab
Loading required package: testthat
Attaching package: ‘testthat’
The following object is masked from ‘package:devtools’:
    setup
The following object is masked from ‘package:purrr’:
    is_null
Testing metahostlab
✔ | OK F W S | Context
⠋ |  1       | 0
══ Results ════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
OK:       1
Failed:   0
Warnings: 0
Skipped:  0
```


A common workflow would be to pull from github, make edits, create new tests for any new functions, and then locally run the tests using the above command. This will save time over the time-intensive Travis-CI official build each time a change is made and can help catch bugs early.


### Uploading initial repository to GitHub
Then I uploaded the repository to github, per instructions [here](https://help.github.com/articles/adding-an-existing-project-to-github-using-the-command-line/). 


### Integrating with Travis.CI

To indicate that a particular R package will use Travis CI, use the following command in R:

```
devtools::use_travis()

* Creating `.travis.yml` from template.
* Adding `.travis.yml` to `.Rbuildignore`.
Next: 
 * Add a travis shield to your README.md:
[![Travis-CI Build Status](https://travis-ci.org/<USERNAME>/<REPO>.svg?branch=master)](https://travis-ci.org/<USERNAME>/<REPO>)
 * Turn on travis for your repo at https://travis-ci.org/<USERNAME>/<REPO>
```

This adds a .travis.yml file with the basic settings to your package root directory. Then, go to [travis website](https://travis-ci.org/account/repositories) and turn on CI for the metahostlab repo.


### Indicate build and test coverage status in GitHub

The covr package in R does code coverage calculations. It can be installed via:
```
install.packages("covr")
```


Then, I added these lines to my .travis.yml file (per instruction [here]( https://github.com/codecov/example-r)
```
r_packages:
  - covr

after_success:
  - Rscript -e 'library(covr); codecov()'
```

When I push to github, it looks like Travis is building...but it turns out that this take forever, so now I see the appeal of locally running 
`devtools::test()`.


In order to get the travis build badge on the repo, you need to add the following code to the top of the README.md file:

```
[![Build Status](your travis url for the repo here.png)](your travis url for the repo here)
```






