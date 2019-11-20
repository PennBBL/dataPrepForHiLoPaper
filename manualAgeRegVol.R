# Load library(s)
source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
source('/home/adrose/dataPrepForHiLoPaper/scripts/functions.R')
#volume.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/volumeDataTest.csv')
volume.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/volumeDataWithAgeAndRestrictionsApplied.csv')


volume.data$age <- scale(volume.data$ageAtGo1Scan)
volume.data$agesq <- scale(volume.data$ageAtGo1Scan)^2
volume.data$agecub <- scale(volume.data$ageAtGo1Scan)^3

mprage_jlf_vol_3rd_Ventricle <- lm(mprage_jlf_vol_3rd_Ventricle ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_4th_Ventricle <- lm(mprage_jlf_vol_4th_Ventricle ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Accumbens <- lm(mprage_jlf_vol_R_Accumbens ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Accumbens <- lm(mprage_jlf_vol_L_Accumbens ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Amygdala <- lm(mprage_jlf_vol_R_Amygdala ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Amygdala <- lm(mprage_jlf_vol_L_Amygdala ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_Brain_Stem <- lm(mprage_jlf_vol_Brain_Stem ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Caudate <- lm(mprage_jlf_vol_R_Caudate ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Caudate <- lm(mprage_jlf_vol_L_Caudate ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Cerebellum_Exterior <- lm(mprage_jlf_vol_R_Cerebellum_Exterior ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Cerebellum_Exterior <- lm(mprage_jlf_vol_L_Cerebellum_Exterior ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Cerebellum_White_Matter <- lm(mprage_jlf_vol_R_Cerebellum_White_Matter ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Cerebellum_White_Matter <- lm(mprage_jlf_vol_L_Cerebellum_White_Matter ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Cerebral_White_Matter <- lm(mprage_jlf_vol_R_Cerebral_White_Matter ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Cerebral_White_Matter <- lm(mprage_jlf_vol_L_Cerebral_White_Matter ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_CSF <- lm(mprage_jlf_vol_CSF ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Hippocampus <- lm(mprage_jlf_vol_R_Hippocampus ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Hippocampus <- lm(mprage_jlf_vol_L_Hippocampus ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Inf_Lat_Vent <- lm(mprage_jlf_vol_R_Inf_Lat_Vent ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Inf_Lat_Vent <- lm(mprage_jlf_vol_L_Inf_Lat_Vent ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Lat_Vent <- lm(mprage_jlf_vol_R_Lat_Vent ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Lat_Vent <- lm(mprage_jlf_vol_L_Lat_Vent ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Pallidum <- lm(mprage_jlf_vol_R_Pallidum ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Pallidum <- lm(mprage_jlf_vol_L_Pallidum ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Putamen <- lm(mprage_jlf_vol_R_Putamen ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Putamen <- lm(mprage_jlf_vol_L_Putamen ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Thal <- lm(mprage_jlf_vol_R_Thal ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Thal <- lm(mprage_jlf_vol_L_Thal ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Vessel <- lm(mprage_jlf_vol_R_Vessel ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Vessel <- lm(mprage_jlf_vol_L_Vessel ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Ventral_DC <- lm(mprage_jlf_vol_R_Ventral_DC ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Ventral_DC <- lm(mprage_jlf_vol_L_Ventral_DC ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_OpticChiasm <- lm(mprage_jlf_vol_OpticChiasm ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_CerVerLobIV <- lm(mprage_jlf_vol_CerVerLobIV ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_CerVerLobIVIVII <- lm(mprage_jlf_vol_CerVerLobIVIVII ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_CerVerLobIVIIIX <- lm(mprage_jlf_vol_CerVerLobIVIIIX ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_BasForebr <- lm(mprage_jlf_vol_L_BasForebr ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_BasForebr <- lm(mprage_jlf_vol_R_BasForebr ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_ACgG <- lm(mprage_jlf_vol_R_ACgG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_ACgG <- lm(mprage_jlf_vol_L_ACgG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Ains <- lm(mprage_jlf_vol_R_Ains ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Ains <- lm(mprage_jlf_vol_L_Ains ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_AOrG <- lm(mprage_jlf_vol_R_AOrG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_AOrG <- lm(mprage_jlf_vol_L_AOrG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_AnG <- lm(mprage_jlf_vol_R_AnG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_AnG <- lm(mprage_jlf_vol_L_AnG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Calc <- lm(mprage_jlf_vol_R_Calc ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Calc <- lm(mprage_jlf_vol_L_Calc ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_CO <- lm(mprage_jlf_vol_R_CO ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_CO <- lm(mprage_jlf_vol_L_CO ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Cun <- lm(mprage_jlf_vol_R_Cun ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Cun <- lm(mprage_jlf_vol_L_Cun ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Ent <- lm(mprage_jlf_vol_R_Ent ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Ent <- lm(mprage_jlf_vol_L_Ent ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_FO <- lm(mprage_jlf_vol_R_FO ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_FO <- lm(mprage_jlf_vol_L_FO ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_FRP <- lm(mprage_jlf_vol_R_FRP ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_FRP <- lm(mprage_jlf_vol_L_FRP ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_FuG <- lm(mprage_jlf_vol_R_FuG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_FuG <- lm(mprage_jlf_vol_L_FuG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Gre <- lm(mprage_jlf_vol_R_Gre ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Gre <- lm(mprage_jlf_vol_L_Gre ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_IOG <- lm(mprage_jlf_vol_R_IOG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_IOG <- lm(mprage_jlf_vol_L_IOG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_ITG <- lm(mprage_jlf_vol_R_ITG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_ITG <- lm(mprage_jlf_vol_L_ITG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_LiG <- lm(mprage_jlf_vol_R_LiG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_LiG <- lm(mprage_jlf_vol_L_LiG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_LOrG <- lm(mprage_jlf_vol_R_LOrG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_LOrG <- lm(mprage_jlf_vol_L_LOrG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_MCgG <- lm(mprage_jlf_vol_R_MCgG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_MCgG <- lm(mprage_jlf_vol_L_MCgG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_MFC <- lm(mprage_jlf_vol_R_MFC ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_MFC <- lm(mprage_jlf_vol_L_MFC ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_MFG <- lm(mprage_jlf_vol_R_MFG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_MFG <- lm(mprage_jlf_vol_L_MFG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_MOG <- lm(mprage_jlf_vol_R_MOG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_MOG <- lm(mprage_jlf_vol_L_MOG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_MOrG <- lm(mprage_jlf_vol_R_MOrG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_MOrG <- lm(mprage_jlf_vol_L_MOrG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_MPoG <- lm(mprage_jlf_vol_R_MPoG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_MPoG <- lm(mprage_jlf_vol_L_MPoG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_MPrG <- lm(mprage_jlf_vol_R_MPrG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_MPrG <- lm(mprage_jlf_vol_L_MPrG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_MSFG <- lm(mprage_jlf_vol_R_MSFG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_MSFG <- lm(mprage_jlf_vol_L_MSFG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_MTG <- lm(mprage_jlf_vol_R_MTG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_MTG <- lm(mprage_jlf_vol_L_MTG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_OCP <- lm(mprage_jlf_vol_R_OCP ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_OCP <- lm(mprage_jlf_vol_L_OCP ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_OFuG <- lm(mprage_jlf_vol_R_OFuG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_OFuG <- lm(mprage_jlf_vol_L_OFuG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_OpIFG <- lm(mprage_jlf_vol_R_OpIFG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_OpIFG <- lm(mprage_jlf_vol_L_OpIFG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_OrIFG <- lm(mprage_jlf_vol_R_OrIFG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_OrIFG <- lm(mprage_jlf_vol_L_OrIFG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_PCgG <- lm(mprage_jlf_vol_R_PCgG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_PCgG <- lm(mprage_jlf_vol_L_PCgG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Pcu <- lm(mprage_jlf_vol_R_Pcu ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Pcu <- lm(mprage_jlf_vol_L_Pcu ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_PHG <- lm(mprage_jlf_vol_R_PHG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_PHG <- lm(mprage_jlf_vol_L_PHG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_Pins <- lm(mprage_jlf_vol_R_Pins ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_Pins <- lm(mprage_jlf_vol_L_Pins ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_PO <- lm(mprage_jlf_vol_R_PO ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_PO <- lm(mprage_jlf_vol_L_PO ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_PoG <- lm(mprage_jlf_vol_R_PoG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_PoG <- lm(mprage_jlf_vol_L_PoG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_POrG <- lm(mprage_jlf_vol_R_POrG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_POrG <- lm(mprage_jlf_vol_L_POrG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_PP <- lm(mprage_jlf_vol_R_PP ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_PP <- lm(mprage_jlf_vol_L_PP ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_PrG <- lm(mprage_jlf_vol_R_PrG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_PrG <- lm(mprage_jlf_vol_L_PrG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_PT <- lm(mprage_jlf_vol_R_PT ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_PT <- lm(mprage_jlf_vol_L_PT ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_SCA <- lm(mprage_jlf_vol_R_SCA ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_SCA <- lm(mprage_jlf_vol_L_SCA ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_SFG <- lm(mprage_jlf_vol_R_SFG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_SFG <- lm(mprage_jlf_vol_L_SFG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_SMC <- lm(mprage_jlf_vol_R_SMC ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_SMC <- lm(mprage_jlf_vol_L_SMC ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_SMG <- lm(mprage_jlf_vol_R_SMG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_SMG <- lm(mprage_jlf_vol_L_SMG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_SOG <- lm(mprage_jlf_vol_R_SOG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_SOG <- lm(mprage_jlf_vol_L_SOG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_SPL <- lm(mprage_jlf_vol_R_SPL ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_SPL <- lm(mprage_jlf_vol_L_SPL ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_STG <- lm(mprage_jlf_vol_R_STG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_STG <- lm(mprage_jlf_vol_L_STG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_TMP <- lm(mprage_jlf_vol_R_TMP ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_TMP <- lm(mprage_jlf_vol_L_TMP ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_TrIFG <- lm(mprage_jlf_vol_R_TrIFG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_TrIFG <- lm(mprage_jlf_vol_L_TrIFG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_R_TTG <- lm(mprage_jlf_vol_R_TTG ~ age + agesq + agecub, data=volume.data)$residuals
mprage_jlf_vol_L_TTG <- lm(mprage_jlf_vol_L_TTG ~ age + agesq + agecub, data=volume.data)$residuals

output <- NULL
output <- cbind(mprage_jlf_vol_3rd_Ventricle,
mprage_jlf_vol_4th_Ventricle,
mprage_jlf_vol_R_Accumbens,
mprage_jlf_vol_L_Accumbens,
mprage_jlf_vol_R_Amygdala,
mprage_jlf_vol_L_Amygdala,
mprage_jlf_vol_Brain_Stem,
mprage_jlf_vol_R_Caudate,
mprage_jlf_vol_L_Caudate,
mprage_jlf_vol_R_Cerebellum_Exterior,
mprage_jlf_vol_L_Cerebellum_Exterior,
mprage_jlf_vol_R_Cerebellum_White_Matter,
mprage_jlf_vol_L_Cerebellum_White_Matter,
mprage_jlf_vol_R_Cerebral_White_Matter,
mprage_jlf_vol_L_Cerebral_White_Matter,
mprage_jlf_vol_CSF,
mprage_jlf_vol_R_Hippocampus,
mprage_jlf_vol_L_Hippocampus,
mprage_jlf_vol_R_Inf_Lat_Vent,
mprage_jlf_vol_L_Inf_Lat_Vent,
mprage_jlf_vol_R_Lat_Vent,
mprage_jlf_vol_L_Lat_Vent,
mprage_jlf_vol_R_Pallidum,
mprage_jlf_vol_L_Pallidum,
mprage_jlf_vol_R_Putamen,
mprage_jlf_vol_L_Putamen,
mprage_jlf_vol_R_Thal,
mprage_jlf_vol_L_Thal,
mprage_jlf_vol_R_Vessel,
mprage_jlf_vol_L_Vessel,
mprage_jlf_vol_R_Ventral_DC,
mprage_jlf_vol_L_Ventral_DC,
mprage_jlf_vol_OpticChiasm,
mprage_jlf_vol_CerVerLobIV,
mprage_jlf_vol_CerVerLobIVIVII,
mprage_jlf_vol_CerVerLobIVIIIX,
mprage_jlf_vol_L_BasForebr,
mprage_jlf_vol_R_BasForebr,
mprage_jlf_vol_R_ACgG,
mprage_jlf_vol_L_ACgG,
mprage_jlf_vol_R_Ains,
mprage_jlf_vol_L_Ains,
mprage_jlf_vol_R_AOrG,
mprage_jlf_vol_L_AOrG,
mprage_jlf_vol_R_AnG,
mprage_jlf_vol_L_AnG,
mprage_jlf_vol_R_Calc,
mprage_jlf_vol_L_Calc,
mprage_jlf_vol_R_CO,
mprage_jlf_vol_L_CO,
mprage_jlf_vol_R_Cun,
mprage_jlf_vol_L_Cun,
mprage_jlf_vol_R_Ent,
mprage_jlf_vol_L_Ent,
mprage_jlf_vol_R_FO,
mprage_jlf_vol_L_FO,
mprage_jlf_vol_R_FRP,
mprage_jlf_vol_L_FRP,
mprage_jlf_vol_R_FuG,
mprage_jlf_vol_L_FuG,
mprage_jlf_vol_R_Gre,
mprage_jlf_vol_L_Gre,
mprage_jlf_vol_R_IOG,
mprage_jlf_vol_L_IOG,
mprage_jlf_vol_R_ITG,
mprage_jlf_vol_L_ITG,
mprage_jlf_vol_R_LiG,
mprage_jlf_vol_L_LiG,
mprage_jlf_vol_R_LOrG,
mprage_jlf_vol_L_LOrG,
mprage_jlf_vol_R_MCgG,
mprage_jlf_vol_L_MCgG,
mprage_jlf_vol_R_MFC,
mprage_jlf_vol_L_MFC,
mprage_jlf_vol_R_MFG,
mprage_jlf_vol_L_MFG,
mprage_jlf_vol_R_MOG,
mprage_jlf_vol_L_MOG,
mprage_jlf_vol_R_MOrG,
mprage_jlf_vol_L_MOrG,
mprage_jlf_vol_R_MPoG,
mprage_jlf_vol_L_MPoG,
mprage_jlf_vol_R_MPrG,
mprage_jlf_vol_L_MPrG,
mprage_jlf_vol_R_MSFG,
mprage_jlf_vol_L_MSFG,
mprage_jlf_vol_R_MTG,
mprage_jlf_vol_L_MTG,
mprage_jlf_vol_R_OCP,
mprage_jlf_vol_L_OCP,
mprage_jlf_vol_R_OFuG,
mprage_jlf_vol_L_OFuG,
mprage_jlf_vol_R_OpIFG,
mprage_jlf_vol_L_OpIFG,
mprage_jlf_vol_R_OrIFG,
mprage_jlf_vol_L_OrIFG,
mprage_jlf_vol_R_PCgG,
mprage_jlf_vol_L_PCgG,
mprage_jlf_vol_R_Pcu,
mprage_jlf_vol_L_Pcu,
mprage_jlf_vol_R_PHG,
mprage_jlf_vol_L_PHG,
mprage_jlf_vol_R_Pins,
mprage_jlf_vol_L_Pins,
mprage_jlf_vol_R_PO,
mprage_jlf_vol_L_PO,
mprage_jlf_vol_R_PoG,
mprage_jlf_vol_L_PoG,
mprage_jlf_vol_R_POrG,
mprage_jlf_vol_L_POrG,
mprage_jlf_vol_R_PP,
mprage_jlf_vol_L_PP,
mprage_jlf_vol_R_PrG,
mprage_jlf_vol_L_PrG,
mprage_jlf_vol_R_PT,
mprage_jlf_vol_L_PT,
mprage_jlf_vol_R_SCA,
mprage_jlf_vol_L_SCA,
mprage_jlf_vol_R_SFG,
mprage_jlf_vol_L_SFG,
mprage_jlf_vol_R_SMC,
mprage_jlf_vol_L_SMC,
mprage_jlf_vol_R_SMG,
mprage_jlf_vol_L_SMG,
mprage_jlf_vol_R_SOG,
mprage_jlf_vol_L_SOG,
mprage_jlf_vol_R_SPL,
mprage_jlf_vol_L_SPL,
mprage_jlf_vol_R_STG,
mprage_jlf_vol_L_STG,
mprage_jlf_vol_R_TMP,
mprage_jlf_vol_L_TMP,
mprage_jlf_vol_R_TrIFG,
mprage_jlf_vol_L_TrIFG,
mprage_jlf_vol_R_TTG,
mprage_jlf_vol_L_TTG)

write.csv(output, 'tmpVals.csv', quote=F, row.names=F)
volume.data[,27:162] <- output


mprage_jlf_vol_R_Accumbens <- (mprage_jlf_vol_L_Accumbens + mprage_jlf_vol_R_Accumbens)/2
mprage_jlf_vol_R_Amygdala <- (mprage_jlf_vol_L_Amygdala + mprage_jlf_vol_R_Amygdala)/2
mprage_jlf_vol_R_Caudate <- (mprage_jlf_vol_L_Caudate + mprage_jlf_vol_R_Caudate)/2
mprage_jlf_vol_R_Cerebellum_Exterior <- (mprage_jlf_vol_L_Cerebellum_Exterior + mprage_jlf_vol_R_Cerebellum_Exterior)/2
mprage_jlf_vol_R_Cerebellum_White_Matter <- (mprage_jlf_vol_L_Cerebellum_White_Matter + mprage_jlf_vol_R_Cerebellum_White_Matter)/2
mprage_jlf_vol_R_Cerebral_White_Matter <- (mprage_jlf_vol_L_Cerebral_White_Matter + mprage_jlf_vol_R_Cerebral_White_Matter)/2
mprage_jlf_vol_R_Hippocampus <- (mprage_jlf_vol_L_Hippocampus + mprage_jlf_vol_R_Hippocampus)/2
mprage_jlf_vol_R_Inf_Lat_Vent <- (mprage_jlf_vol_L_Inf_Lat_Vent + mprage_jlf_vol_R_Inf_Lat_Vent)/2
mprage_jlf_vol_R_Lat_Vent <- (mprage_jlf_vol_L_Lat_Vent + mprage_jlf_vol_R_Lat_Vent)/2
mprage_jlf_vol_R_Pallidum <- (mprage_jlf_vol_L_Pallidum + mprage_jlf_vol_R_Pallidum)/2
mprage_jlf_vol_R_Putamen <- (mprage_jlf_vol_L_Putamen + mprage_jlf_vol_R_Putamen)/2
mprage_jlf_vol_R_Thal <- (mprage_jlf_vol_L_Thal + mprage_jlf_vol_R_Thal)/2
mprage_jlf_vol_R_Vessel <- (mprage_jlf_vol_L_Vessel + mprage_jlf_vol_R_Vessel)/2
mprage_jlf_vol_R_Ventral_DC <- (mprage_jlf_vol_L_Ventral_DC + mprage_jlf_vol_R_Ventral_DC)/2
mprage_jlf_vol_R_BasForebr <- (mprage_jlf_vol_L_BasForebr + mprage_jlf_vol_R_BasForebr)/2
mprage_jlf_vol_R_ACgG <- (mprage_jlf_vol_L_ACgG + mprage_jlf_vol_R_ACgG)/2
mprage_jlf_vol_R_Ains <- (mprage_jlf_vol_L_Ains + mprage_jlf_vol_R_Ains)/2
mprage_jlf_vol_R_AOrG <- (mprage_jlf_vol_L_AOrG + mprage_jlf_vol_R_AOrG)/2
mprage_jlf_vol_R_AnG <- (mprage_jlf_vol_L_AnG + mprage_jlf_vol_R_AnG)/2
mprage_jlf_vol_R_Calc <- (mprage_jlf_vol_L_Calc + mprage_jlf_vol_R_Calc)/2
mprage_jlf_vol_R_CO <- (mprage_jlf_vol_L_CO + mprage_jlf_vol_R_CO)/2
mprage_jlf_vol_R_Cun <- (mprage_jlf_vol_L_Cun + mprage_jlf_vol_R_Cun)/2
mprage_jlf_vol_R_Ent <- (mprage_jlf_vol_L_Ent + mprage_jlf_vol_R_Ent)/2
mprage_jlf_vol_R_FO <- (mprage_jlf_vol_L_FO + mprage_jlf_vol_R_FO)/2
mprage_jlf_vol_R_FRP <- (mprage_jlf_vol_L_FRP + mprage_jlf_vol_R_FRP)/2
mprage_jlf_vol_R_FuG <- (mprage_jlf_vol_L_FuG + mprage_jlf_vol_R_FuG)/2
mprage_jlf_vol_R_Gre <- (mprage_jlf_vol_L_Gre + mprage_jlf_vol_R_Gre)/2
mprage_jlf_vol_R_IOG <- (mprage_jlf_vol_L_IOG + mprage_jlf_vol_R_IOG)/2
mprage_jlf_vol_R_ITG <- (mprage_jlf_vol_L_ITG + mprage_jlf_vol_R_ITG)/2
mprage_jlf_vol_R_LiG <- (mprage_jlf_vol_L_LiG + mprage_jlf_vol_R_LiG)/2
mprage_jlf_vol_R_LOrG <- (mprage_jlf_vol_L_LOrG + mprage_jlf_vol_R_LOrG)/2
mprage_jlf_vol_R_MCgG <- (mprage_jlf_vol_L_MCgG + mprage_jlf_vol_R_MCgG)/2
mprage_jlf_vol_R_MFC <- (mprage_jlf_vol_L_MFC + mprage_jlf_vol_R_MFC)/2
mprage_jlf_vol_R_MFG <- (mprage_jlf_vol_L_MFG + mprage_jlf_vol_R_MFG)/2
mprage_jlf_vol_R_MOG <- (mprage_jlf_vol_L_MOG + mprage_jlf_vol_R_MOG)/2
mprage_jlf_vol_R_MOrG <- (mprage_jlf_vol_L_MOrG + mprage_jlf_vol_R_MOrG)/2
mprage_jlf_vol_R_MPoG <- (mprage_jlf_vol_L_MPoG + mprage_jlf_vol_R_MPoG)/2
mprage_jlf_vol_R_MPrG <- (mprage_jlf_vol_L_MPrG + mprage_jlf_vol_R_MPrG)/2
mprage_jlf_vol_R_MSFG <- (mprage_jlf_vol_L_MSFG + mprage_jlf_vol_R_MSFG)/2
mprage_jlf_vol_R_MTG <- (mprage_jlf_vol_L_MTG + mprage_jlf_vol_R_MTG)/2
mprage_jlf_vol_R_OCP <- (mprage_jlf_vol_L_OCP + mprage_jlf_vol_R_OCP)/2
mprage_jlf_vol_R_OFuG <- (mprage_jlf_vol_L_OFuG + mprage_jlf_vol_R_OFuG)/2
mprage_jlf_vol_R_OpIFG <- (mprage_jlf_vol_L_OpIFG + mprage_jlf_vol_R_OpIFG)/2
mprage_jlf_vol_R_OrIFG <- (mprage_jlf_vol_L_OrIFG + mprage_jlf_vol_R_OrIFG)/2
mprage_jlf_vol_R_PCgG <- (mprage_jlf_vol_L_PCgG + mprage_jlf_vol_R_PCgG)/2
mprage_jlf_vol_R_Pcu <- (mprage_jlf_vol_L_Pcu + mprage_jlf_vol_R_Pcu)/2
mprage_jlf_vol_R_PHG <- (mprage_jlf_vol_L_PHG + mprage_jlf_vol_R_PHG)/2
mprage_jlf_vol_R_Pins <- (mprage_jlf_vol_L_Pins + mprage_jlf_vol_R_Pins)/2
mprage_jlf_vol_R_PO <- (mprage_jlf_vol_L_PO + mprage_jlf_vol_R_PO)/2
mprage_jlf_vol_R_PoG <- (mprage_jlf_vol_L_PoG + mprage_jlf_vol_R_PoG)/2
mprage_jlf_vol_R_POrG <- (mprage_jlf_vol_L_POrG + mprage_jlf_vol_R_POrG)/2
mprage_jlf_vol_R_PP <- (mprage_jlf_vol_L_PP + mprage_jlf_vol_R_PP)/2
mprage_jlf_vol_R_PrG <- (mprage_jlf_vol_L_PrG + mprage_jlf_vol_R_PrG)/2
mprage_jlf_vol_R_PT <- (mprage_jlf_vol_L_PT + mprage_jlf_vol_R_PT)/2
mprage_jlf_vol_R_SCA <- (mprage_jlf_vol_L_SCA + mprage_jlf_vol_R_SCA)/2
mprage_jlf_vol_R_SFG <- (mprage_jlf_vol_L_SFG + mprage_jlf_vol_R_SFG)/2
mprage_jlf_vol_R_SMC <- (mprage_jlf_vol_L_SMC + mprage_jlf_vol_R_SMC)/2
mprage_jlf_vol_R_SMG <- (mprage_jlf_vol_L_SMG + mprage_jlf_vol_R_SMG)/2
mprage_jlf_vol_R_SOG <- (mprage_jlf_vol_L_SOG + mprage_jlf_vol_R_SOG)/2
mprage_jlf_vol_R_SPL <- (mprage_jlf_vol_L_SPL + mprage_jlf_vol_R_SPL)/2
mprage_jlf_vol_R_STG <- (mprage_jlf_vol_L_STG + mprage_jlf_vol_R_STG)/2
mprage_jlf_vol_R_TMP <- (mprage_jlf_vol_L_TMP + mprage_jlf_vol_R_TMP)/2
mprage_jlf_vol_R_TrIFG <- (mprage_jlf_vol_L_TrIFG + mprage_jlf_vol_R_TrIFG)/2
mprage_jlf_vol_R_TTG <- (mprage_jlf_vol_L_TTG + mprage_jlf_vol_R_TTG)/2

meanedValues <- cbind(mprage_jlf_vol_R_Accumbens,
mprage_jlf_vol_R_Amygdala,
mprage_jlf_vol_R_Caudate,
mprage_jlf_vol_R_Cerebellum_Exterior,
mprage_jlf_vol_R_Cerebellum_White_Matter,
mprage_jlf_vol_R_Cerebral_White_Matter,
mprage_jlf_vol_R_Hippocampus,
mprage_jlf_vol_R_Inf_Lat_Vent,
mprage_jlf_vol_R_Lat_Vent,
mprage_jlf_vol_R_Pallidum,
mprage_jlf_vol_R_Putamen,
mprage_jlf_vol_R_Thal,
mprage_jlf_vol_R_Vessel,
mprage_jlf_vol_R_Ventral_DC,
mprage_jlf_vol_R_BasForebr,
mprage_jlf_vol_R_ACgG,
mprage_jlf_vol_R_Ains,
mprage_jlf_vol_R_AOrG,
mprage_jlf_vol_R_AnG,
mprage_jlf_vol_R_Calc,
mprage_jlf_vol_R_CO,
mprage_jlf_vol_R_Cun,
mprage_jlf_vol_R_Ent,
mprage_jlf_vol_R_FO,
mprage_jlf_vol_R_FRP,
mprage_jlf_vol_R_FuG,
mprage_jlf_vol_R_Gre,
mprage_jlf_vol_R_IOG,
mprage_jlf_vol_R_ITG,
mprage_jlf_vol_R_LiG,
mprage_jlf_vol_R_LOrG,
mprage_jlf_vol_R_MCgG,
mprage_jlf_vol_R_MFC,
mprage_jlf_vol_R_MFG,
mprage_jlf_vol_R_MOG,
mprage_jlf_vol_R_MOrG,
mprage_jlf_vol_R_MPoG,
mprage_jlf_vol_R_MPrG,
mprage_jlf_vol_R_MSFG,
mprage_jlf_vol_R_MTG,
mprage_jlf_vol_R_OCP,
mprage_jlf_vol_R_OFuG,
mprage_jlf_vol_R_OpIFG,
mprage_jlf_vol_R_OrIFG,
mprage_jlf_vol_R_PCgG,
mprage_jlf_vol_R_Pcu,
mprage_jlf_vol_R_PHG,
mprage_jlf_vol_R_Pins,
mprage_jlf_vol_R_PO,
mprage_jlf_vol_R_PoG,
mprage_jlf_vol_R_POrG,
mprage_jlf_vol_R_PP,
mprage_jlf_vol_R_PrG,
mprage_jlf_vol_R_PT,
mprage_jlf_vol_R_SCA,
mprage_jlf_vol_R_SFG,
mprage_jlf_vol_R_SMC,
mprage_jlf_vol_R_SMG,
mprage_jlf_vol_R_SOG,
mprage_jlf_vol_R_SPL,
mprage_jlf_vol_R_STG,
mprage_jlf_vol_R_TMP,
mprage_jlf_vol_R_TrIFG,
mprage_jlf_vol_R_TTG)

volume.data <- volume.data[,-grep('_L_', names(volume.data))]
volume.data[,grep('_R_', names(volume.data))] <- meanedValues
colnames(volume.data) <- gsub(pattern='_R_', replacement='', x=colnames(volume.data), fixed=TRUE)

tmpOutput <- volume.data
volume.data <- volume.data[,27:98]
# Now perform wihtin modality regression
mprage_jlf_vol_3rd_Ventricle <- lm(mprage_jlf_vol_3rd_Ventricle ~ ., data=volume.data)$residuals
mprage_jlf_vol_4th_Ventricle <- lm(mprage_jlf_vol_4th_Ventricle ~ ., data=volume.data)$residuals
mprage_jlf_volAccumbens <- lm(mprage_jlf_volAccumbens ~ ., data=volume.data)$residuals
mprage_jlf_volAmygdala <- lm(mprage_jlf_volAmygdala ~ ., data=volume.data)$residuals
mprage_jlf_vol_Brain_Stem <- lm(mprage_jlf_vol_Brain_Stem ~ ., data=volume.data)$residuals
mprage_jlf_volCaudate <- lm(mprage_jlf_volCaudate ~ ., data=volume.data)$residuals
mprage_jlf_volCerebellum_Exterior <- lm(mprage_jlf_volCerebellum_Exterior ~ ., data=volume.data)$residuals
mprage_jlf_volCerebellum_White_Matter <- lm(mprage_jlf_volCerebellum_White_Matter ~ ., data=volume.data)$residuals
mprage_jlf_volCerebral_White_Matter <- lm(mprage_jlf_volCerebral_White_Matter ~ ., data=volume.data)$residuals
mprage_jlf_vol_CSF <- lm(mprage_jlf_vol_CSF ~ ., data=volume.data)$residuals
mprage_jlf_volHippocampus <- lm(mprage_jlf_volHippocampus ~ ., data=volume.data)$residuals
mprage_jlf_volInf_Lat_Vent <- lm(mprage_jlf_volInf_Lat_Vent ~ ., data=volume.data)$residuals
mprage_jlf_volLat_Vent <- lm(mprage_jlf_volLat_Vent ~ ., data=volume.data)$residuals
mprage_jlf_volPallidum <- lm(mprage_jlf_volPallidum ~ ., data=volume.data)$residuals
mprage_jlf_volPutamen <- lm(mprage_jlf_volPutamen ~ ., data=volume.data)$residuals
mprage_jlf_volThal <- lm(mprage_jlf_volThal ~ ., data=volume.data)$residuals
mprage_jlf_volVessel <- lm(mprage_jlf_volVessel ~ ., data=volume.data)$residuals
mprage_jlf_volVentral_DC <- lm(mprage_jlf_volVentral_DC ~ ., data=volume.data)$residuals
mprage_jlf_vol_OpticChiasm <- lm(mprage_jlf_vol_OpticChiasm ~ ., data=volume.data)$residuals
mprage_jlf_vol_CerVerLobIV <- lm(mprage_jlf_vol_CerVerLobIV ~ ., data=volume.data)$residuals
mprage_jlf_vol_CerVerLobIVIVII <- lm(mprage_jlf_vol_CerVerLobIVIVII ~ ., data=volume.data)$residuals
mprage_jlf_vol_CerVerLobIVIIIX <- lm(mprage_jlf_vol_CerVerLobIVIIIX ~ ., data=volume.data)$residuals
mprage_jlf_volBasForebr <- lm(mprage_jlf_volBasForebr ~ ., data=volume.data)$residuals
mprage_jlf_volACgG <- lm(mprage_jlf_volACgG ~ ., data=volume.data)$residuals
mprage_jlf_volAins <- lm(mprage_jlf_volAins ~ ., data=volume.data)$residuals
mprage_jlf_volAOrG <- lm(mprage_jlf_volAOrG ~ ., data=volume.data)$residuals
mprage_jlf_volAnG <- lm(mprage_jlf_volAnG ~ ., data=volume.data)$residuals
mprage_jlf_volCalc <- lm(mprage_jlf_volCalc ~ ., data=volume.data)$residuals
mprage_jlf_volCO <- lm(mprage_jlf_volCO ~ ., data=volume.data)$residuals
mprage_jlf_volCun <- lm(mprage_jlf_volCun ~ ., data=volume.data)$residuals
mprage_jlf_volEnt <- lm(mprage_jlf_volEnt ~ ., data=volume.data)$residuals
mprage_jlf_volFO <- lm(mprage_jlf_volFO ~ ., data=volume.data)$residuals
mprage_jlf_volFRP <- lm(mprage_jlf_volFRP ~ ., data=volume.data)$residuals
mprage_jlf_volFuG <- lm(mprage_jlf_volFuG ~ ., data=volume.data)$residuals
mprage_jlf_volGre <- lm(mprage_jlf_volGre ~ ., data=volume.data)$residuals
mprage_jlf_volIOG <- lm(mprage_jlf_volIOG ~ ., data=volume.data)$residuals
mprage_jlf_volITG <- lm(mprage_jlf_volITG ~ ., data=volume.data)$residuals
mprage_jlf_volLiG <- lm(mprage_jlf_volLiG ~ ., data=volume.data)$residuals
mprage_jlf_volLOrG <- lm(mprage_jlf_volLOrG ~ ., data=volume.data)$residuals
mprage_jlf_volMCgG <- lm(mprage_jlf_volMCgG ~ ., data=volume.data)$residuals
mprage_jlf_volMFC <- lm(mprage_jlf_volMFC ~ ., data=volume.data)$residuals
mprage_jlf_volMFG <- lm(mprage_jlf_volMFG ~ ., data=volume.data)$residuals
mprage_jlf_volMOG <- lm(mprage_jlf_volMOG ~ ., data=volume.data)$residuals
mprage_jlf_volMOrG <- lm(mprage_jlf_volMOrG ~ ., data=volume.data)$residuals
mprage_jlf_volMPoG <- lm(mprage_jlf_volMPoG ~ ., data=volume.data)$residuals
mprage_jlf_volMPrG <- lm(mprage_jlf_volMPrG ~ ., data=volume.data)$residuals
mprage_jlf_volMSFG <- lm(mprage_jlf_volMSFG ~ ., data=volume.data)$residuals
mprage_jlf_volMTG <- lm(mprage_jlf_volMTG ~ ., data=volume.data)$residuals
mprage_jlf_volOCP <- lm(mprage_jlf_volOCP ~ ., data=volume.data)$residuals
mprage_jlf_volOFuG <- lm(mprage_jlf_volOFuG ~ ., data=volume.data)$residuals
mprage_jlf_volOpIFG <- lm(mprage_jlf_volOpIFG ~ ., data=volume.data)$residuals
mprage_jlf_volOrIFG <- lm(mprage_jlf_volOrIFG ~ ., data=volume.data)$residuals
mprage_jlf_volPCgG <- lm(mprage_jlf_volPCgG ~ ., data=volume.data)$residuals
mprage_jlf_volPcu <- lm(mprage_jlf_volPcu ~ ., data=volume.data)$residuals
mprage_jlf_volPHG <- lm(mprage_jlf_volPHG ~ ., data=volume.data)$residuals
mprage_jlf_volPins <- lm(mprage_jlf_volPins ~ ., data=volume.data)$residuals
mprage_jlf_volPO <- lm(mprage_jlf_volPO ~ ., data=volume.data)$residuals
mprage_jlf_volPoG <- lm(mprage_jlf_volPoG ~ ., data=volume.data)$residuals
mprage_jlf_volPOrG <- lm(mprage_jlf_volPOrG ~ ., data=volume.data)$residuals
mprage_jlf_volPP <- lm(mprage_jlf_volPP ~ ., data=volume.data)$residuals
mprage_jlf_volPrG <- lm(mprage_jlf_volPrG ~ ., data=volume.data)$residuals
mprage_jlf_volPT <- lm(mprage_jlf_volPT ~ ., data=volume.data)$residuals
mprage_jlf_volSCA <- lm(mprage_jlf_volSCA ~ ., data=volume.data)$residuals
mprage_jlf_volSFG <- lm(mprage_jlf_volSFG ~ ., data=volume.data)$residuals
mprage_jlf_volSMC <- lm(mprage_jlf_volSMC ~ ., data=volume.data)$residuals
mprage_jlf_volSMG <- lm(mprage_jlf_volSMG ~ ., data=volume.data)$residuals
mprage_jlf_volSOG <- lm(mprage_jlf_volSOG ~ ., data=volume.data)$residuals
mprage_jlf_volSPL <- lm(mprage_jlf_volSPL ~ ., data=volume.data)$residuals
mprage_jlf_volSTG <- lm(mprage_jlf_volSTG ~ ., data=volume.data)$residuals
mprage_jlf_volTMP <- lm(mprage_jlf_volTMP ~ ., data=volume.data)$residuals
mprage_jlf_volTrIFG <- lm(mprage_jlf_volTrIFG ~ ., data=volume.data)$residuals
mprage_jlf_volTTG <- lm(mprage_jlf_volTTG ~ ., data=volume.data)$residuals


modalRegressVals <- cbind(mprage_jlf_vol_3rd_Ventricle,
mprage_jlf_vol_4th_Ventricle,
mprage_jlf_volAccumbens,
mprage_jlf_volAmygdala,
mprage_jlf_vol_Brain_Stem,
mprage_jlf_volCaudate,
mprage_jlf_volCerebellum_Exterior,
mprage_jlf_volCerebellum_White_Matter,
mprage_jlf_volCerebral_White_Matter,
mprage_jlf_vol_CSF,
mprage_jlf_volHippocampus,
mprage_jlf_volInf_Lat_Vent,
mprage_jlf_volLat_Vent,
mprage_jlf_volPallidum,
mprage_jlf_volPutamen,
mprage_jlf_volThal,
mprage_jlf_volVessel,
mprage_jlf_volVentral_DC,
mprage_jlf_vol_OpticChiasm,
mprage_jlf_vol_CerVerLobIV,
mprage_jlf_vol_CerVerLobIVIVII,
mprage_jlf_vol_CerVerLobIVIIIX,
mprage_jlf_volBasForebr,
mprage_jlf_volACgG,
mprage_jlf_volAins,
mprage_jlf_volAOrG,
mprage_jlf_volAnG,
mprage_jlf_volCalc,
mprage_jlf_volCO,
mprage_jlf_volCun,
mprage_jlf_volEnt,
mprage_jlf_volFO,
mprage_jlf_volFRP,
mprage_jlf_volFuG,
mprage_jlf_volGre,
mprage_jlf_volIOG,
mprage_jlf_volITG,
mprage_jlf_volLiG,
mprage_jlf_volLOrG,
mprage_jlf_volMCgG,
mprage_jlf_volMFC,
mprage_jlf_volMFG,
mprage_jlf_volMOG,
mprage_jlf_volMOrG,
mprage_jlf_volMPoG,
mprage_jlf_volMPrG,
mprage_jlf_volMSFG,
mprage_jlf_volMTG,
mprage_jlf_volOCP,
mprage_jlf_volOFuG,
mprage_jlf_volOpIFG,
mprage_jlf_volOrIFG,
mprage_jlf_volPCgG,
mprage_jlf_volPcu,
mprage_jlf_volPHG,
mprage_jlf_volPins,
mprage_jlf_volPO,
mprage_jlf_volPoG,
mprage_jlf_volPOrG,
mprage_jlf_volPP,
mprage_jlf_volPrG,
mprage_jlf_volPT,
mprage_jlf_volSCA,
mprage_jlf_volSFG,
mprage_jlf_volSMC,
mprage_jlf_volSMG,
mprage_jlf_volSOG,
mprage_jlf_volSPL,
mprage_jlf_volSTG,
mprage_jlf_volTMP,
mprage_jlf_volTrIFG,
mprage_jlf_volTTG)
volume.data <- tmpOutput
volume.data[,27:98] <- modalRegressVals
write.csv(volume.data, 'modalRegressedVolumeData.csv', quote=F, row.names=F)
