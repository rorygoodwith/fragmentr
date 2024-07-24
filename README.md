# fragmentr
Graded Fragmentation Effects for Magick Images in R

Fragmentr is an R package with one function, `image_fragment`, which copies an image a set number of times and partially obscures the foreground of each copy with coloured squares. The proportion of each image copy that is obscured by these white squares increases according to a power function, meaning the resulting image copies range from completely unobscured to almost totally obscured (see example below).

![image](https://user-images.githubusercontent.com/51744937/170519468-75da8789-abde-4e75-8126-0a89ed6893fc.png)


These images are often used in cognitive psychology experiments where participants gradually reveal an obscured stimulus in an attempt to identify it at the highest level of obscurity possible (e.g. [Berry et al., 2014](https://www.jneurosci.org/content/jneuro/34/33/10963.full.pdf)).

## Installation
You can install Fragmentr with the following R code:

```devtools::install_github("rorygoodwith/fragmentr")```
