# Internal data for checking column headings

## Essential data for recalculation
essential_aci_cols <- c(
  "Area",
  "StmRat",
  "Tair",
  "Tleaf",
  "TBlk",
  "CO2R",
  "CO2S",
  "H2OR",
  "H2OS",
  "Flow",
  "PARi",
  "Press"
)

## Nonessential default columns
default_aci_cols <- c(
  "FTime",
  "EBal?",
  "A",
  "gs", 
  "Ci", 
  "Trmmol",  
  "VpdL", 
  "CTleaf", 
  "BLC_1",
  "BLCond",
  "RH_R", 
  "RH_S", 
  "PARo",
  "CsMch", 
  "HsMch",
  "StableF",
  "BLCslope", 
  "BLCoffst",
  "f_parin",
  "f_parout", 
  "alphaK",
  "Status",
  "fda",
  "Trans",
  "Tair_K",
  "Twall_K", 
  "R(W/m2)",
  "Tl-Ta",
  "SVTleaf",
  "h2o_i",
  "h20diff",
  "CTair",
  "SVTair",
  "CndTotal", 
  "vp_kPa", 
  "VpdA",
  "CndCO2",
  "Ci_Pa",
  "Ci/Ca",
  "RHsfc",
  "C2sfc",
  "AHs/Cs"  
)

usethis::use_data(essential_aci_cols, default_aci_cols, internal = TRUE,
                  overwrite = TRUE)