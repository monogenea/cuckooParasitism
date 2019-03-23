# Bayesian modelling tutorial

## Instructions

1. Install `rethinking`, `greta` and their dependencies TensorFlow and RStan

The `rethinking` package requires `rstan` which might require you somewhat special compiling instructions (ADD RENVIRON LINE HERE!).

The `greta` in turn, requires the Python modules `tensorflow` and `tensorflow_probability`. You can either

*i*) use the following code in a fresh terminal,
```{bash}
conda create --name greta
conda activate greta
conda install tensorflow==1.12.0
conda install tensorflow-probability
conda update --all
conda env export --no-builds > greta.yml
```
to create a Conda environment called `greta` (the last line is optional), or

*ii*) borrow my `greta.yml` file to copy my `greta` Conda enviroment,
```{bash}
conda env create -f greta.yml
```

2. Install all packages listed on top of the script. BioConductor??

3. Run `script.R`. The TensorFlow instalation defaults to CPU; alternatively you can use NVIDIA GPUs (if you have them) which run substantially faster.

## Acknowledgements

This work is partly based on:

- Original publication - [*Social parasitism as an alternative reproductive tactic in a cooperatively breeding cuckoo*](https://www.nature.com/articles/s41586-019-0981-1), from Riehl *et al*., 2019 

Enjoy, all feedback is welcome!

Francisco
