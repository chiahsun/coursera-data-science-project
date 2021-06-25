Since test files include other files by source(...), we have to change 

working directory under `tests` and then execute:

```R
testthat::test_dir(".")
```