# Bayesian modelling tutorial

## Instructions

1. Install `rethinking`, `greta` and their dependencies TensorFlow and RStan

The `rethinking` package requires `rstan` which might require you somewhat special compiling instructions. If you have trouble with the C++ compiler try creating (or editing) the file `~/.R/Makevars` containing the line `CXX14 = g++ -std=c++1y -Wno-unused-variable -Wno-unused-function -fPIC`.

The `greta` in turn, requires the Python modules `tensorflow` and `tensorflow_probability`. I propose using CPU-only TensorFlow within Conda environments, but feel free to use alternatives at your own discretion. You can either:

* *i*) use the following code in a fresh terminal,
```{bash}
conda create --name greta
conda activate greta
conda install tensorflow==1.12.0
conda install tensorflow-probability
conda update --all
```
to create a Conda environment called `greta`, or

* *ii*) borrow my `greta.yml` file to copy my `greta` Conda enviroment,
```{bash}
conda env create -f greta.yml
```
Quick side note: to produce this file you can use the command `conda env export --no-builds > greta.yml`.

2. Install all packages listed on top of the two R scripts `zipoisEggsFledged.R` and `dbinomSuccessful.R`. All of them are available on CRAN.

3. Run either or both scripts - the first implements a `rethinking`-based zero-inflated Poisson regression on the number of fledglings, whereas the second implements a `greta`-based logistic regression on whether a female has successfully produced fledglings or not.

## Acknowledgements

The dataset used in this work pertains to the publication:

- [Riehl, C. & Strong, M. J. Social parasitism as an alternative reproductive tactic in a cooperatively breeding cuckoo. *Nature* **567**, 96-99 (2019)](https://www.nature.com/articles/s41586-019-0981-1)

I want to thank the author Christina Riehl for all insighful discussions and suggestions. I also want to thank Nick Golding and the community at the [`greta`forum](https://forum.greta-stats.org) for their kind contributions.

Enjoy, all feedback is welcome!

Francisco
