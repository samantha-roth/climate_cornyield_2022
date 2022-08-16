# climate_cornyield_2022

Test mesh performances using CVRMSE ensuring that each county has a fixed effect estimated:
1. Run crossvalidatonAllYrsTryMeshes.R (to fit meshes and compute AMat, eigenvectors of Moran's operator)
2. Run testMeshPerformancesCVRMSECutoff.k.R for k in (1,....,7)
3. Run compareMeshDensitiesCVRMSE.R (to compare cutoffs in terms of CVRMSE at different rank(M)s)

Test mesh performances using BIC (no heldout data):
1. Run tryMeshes.R (to fit meshes and compute AMat, eigenvectors of Moran's operator)
2. Run testMeshPerformancesCutoff.k.R for k in (1,....,7)
3. Run compareMeshDensitiesBIC.R (to compare cutoffs in terms of BIC at different rank(M)s)
