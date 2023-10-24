# MRes-Mangrove-Height-Detection

## Table of Contents
- [Introduction](#introduction)
- [Data Sources](#data-sources)
- [Code](#code)
- [Usage](#usage)
- [License](#license)

# Introduction

This repository contains code that investigates mangrove heights by utilizing GEDI data. As part of the project, Sentinel-2 imagery was employed to extrapolate GEDI 25m resolution shots to create a Mangrove Canopy Height Map (CHM).

# Data Sources

| Data Source                   | Platform                                     | Resolution |
|-------------------------------|----------------------------------------------|------------|
| Sentinel-2 Imagery            | [Google Earth Engine](https://code.earthengine.google.com/) | 10m (RGB), 20m (SWIR), 60m (thermal), Resampled to 25m |
| GEDI (ICESat-2) Data          | [NASA Earth Data Search](https://search.earthdata.nasa.gov/search?q=GEDI%20L1B&sb[0]=94.37695%2C17.00659%2C94.58789%2C17.21311&fst0=Land%20Surface&fst1=Biosphere&lat=17.05078125&long=93.0146484375&zoom=7) | ~25m (footprint size) |
| Comparator CHMs               |                                               |            |
| Simard-2000                   | [ORNL DAAC](https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1665) | 30m         |
| Lang-2021                     | [GitHub](https://langnico.github.io/globalcanopyheight/) | 10m         |
| Potapov-2019                  | [Global Land Analysis and Discovery (GLAD)](https://glad.umd.edu/dataset/gedi/) | 30m        |

# Code

The code is separated into three parts:

a) Generic investigation of GEDI L1A products in Python

b) Training GEDI L2A model with Sentinel-2

c) Code to compare all results

We compare the predictions of the four final models for 2019 heights.


<img width="259.5" alt="Comparison of Models" src="https://github.com/excitedmuck/MRes-Mangrove-Height-Detection/assets/33532101/1f68a6cd-bba7-48b9-ac45-2b62cda1f505">

# Usage

Detecting changes in mangrove height in Project VCS1764, Ayeyarwadi, Myanmar.


<img width="259.5" alt="Mangrove Height Detection" src="https://github.com/excitedmuck/MRes-Mangrove-Height-Detection/assets/33532101/de096cc5-65fd-44bc-bcc8-1e86eff7c57a">

# License
MIT

# Citation

Shukla Y., (2023). Mangrove Canopy Height Map for VCS1764 in Myanmar, 2019.
