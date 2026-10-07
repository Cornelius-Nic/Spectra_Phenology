**Reflectance spectra capture temporal variation in functional traits and their integration across leaf phenology**

This repository supports this manuscript.

**Preprint**: Nichodemus, CO and Meireles, JE (2026). [Reflectance spectra capture temporal variation in functional traits and leaf phenology]((https://www.biorxiv.org/content/10.64898/2026.04.21.719921v1)). bioRxiv https://www.biorxiv.org/content/10.64898/2026.04.21.719921v1 (Under review in _New Phytologist_).

In this paper, we showed
(i) the temporal variation in spectra, functional traits, and their integration (correlation) across leaf phenology. 
(ii) the importance of building trait prediction models that are representative across leaf phenology. This is useful because such models outperform models trained on a narrow time slice (a single phenophase, e.g., mature leaf only).
(iii) that ignoring temporal variability in leaf phenology and traits can systematically bias trait estimates and ecological inference. 
Additionally, we built three trait models that cover different phenological stages, namely, Complete leaf phenology models (_all-season_ and _week-as-covariate_) and Single leaf phenophase model (_peak-season_), and assessed their performance alongside a widely used trait model (Kothari et al. 2023; New Phytologist).

# How to use
This script is split into 3 parts: 
1. Data processing: preprocessing (cleaning and trimming) of spectral data, calculation of derived traits (LMA and EWT), and merging of processed spectra with traits. The traits include LMA, EWT, C, N, and QY (Quantum yield).
2. Data analysis: building predictive trait models, assessing model performance, spectral and trait phenology, testing for statistically significant differences in traits across phenophases, and assessing how reasonable trait predictions are from existing models.
3. Utils: functions developed for this study.

Feel free to contact **Cornelius-Nic** if you have questions. 
If you find this script useful for your research, kindly cite the above paper.
