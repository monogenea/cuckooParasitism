# Bayesian modelling tutorial

## Instructions

1. Install `rethinking`, `greta` and their dependencies TensorFlow and RStan

The `rethinking` package requires `rstan` which might require you somewhat special compiling instructions (ADD RENVIRON LINE HERE!).

The `greta` in turn, requires the Python modules `tensorflow` and `tensorflow_probability`. You can either

**i*) paste the following Bash code onto a fresh terminal,
```{bash}
conda create --name greta
conda activate greta
conda install tensorflow==1.12.0
conda install tensorflow-probability
conda update --all
conda env export --no-builds > greta.yml
```
to create a Conda environment called `greta` (the last line is optional), or

**ii*) borrow my `greta.yml` file to copy my `greta` Conda enviroment,
```{bash}
conda env create -f greta.yml
```

2. Install all packages listed on top of the script. BioConductor??

3. Run `script.R`. The TensorFlow instalation defaults to CPU; alternatively you can use NVIDIA GPUs (if you have them) which run substantially faster.

## Acknowledgements

This work is partly based on:

- Image processing - [*Image Classification in R: MXNet*](https://rpubs.com/kanedglsk/236125) from Shikun Li 
- CNN architecture - Keras [CIFAR-10 CNN example](https://github.com/keras-team/keras/blob/master/examples/cifar10_cnn.py)

Enjoy, all feedback is welcome!

Francisco
