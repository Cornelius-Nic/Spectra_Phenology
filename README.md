**Reflectance spectra capture temporal variation in functional traits and leaf phenology**

This repository is designed to support this manuscript.

**Preprint**: Nichodemus, CO and Meireles, JE (2026). [Reflectance spectra capture temporal variation in functional traits and leaf phenology]((https://www.biorxiv.org/content/10.64898/2026.04.21.719921v1)). bioRxiv https://www.biorxiv.org/content/10.64898/2026.04.21.719921v1 (Under review in _New Phytologist_).

In this paper, the authors showed the importance of building trait prediction models that cover temporal variability in leaf phenology and functional traits. Such models outperform models trained on a narrow slice of time (a single phenophase), and ignoring temporal variability in leaf phenology and traits can systematically bias trait estimates and ecological inference. The authors built three trait models, namely _all-season_, _week-as-covariate_, and _peak-season_. Additionally, the authors included a widely used trait model - Kothari et al. 2023 (New Phytologist).

# How to use
This script is split into 3 parts: 
1. Data processing: cleaning, trimming of raw data, calculation of derived traits (LMA and EWT), and merging of processed spectra with traits. The traits include LMA, EWT, C, and N.
2. Data analysis: building predictive trait models, assessing model performance, trait time series plots, spectral plots, other statistical analyses to test significance differences in traits across phenophases, and how reasonable trait predictions are from existing models.
3. Utils: functions developed for this study.

Feel free to contact **Cornelius-Nic** if you have questions. 
If you find this script useful for your research, kindly cite the above paper.
