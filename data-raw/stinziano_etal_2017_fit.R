# EXAMPLE OUTPUT FOR basic-racir.Rmd. STILL WORKING ON THIS
df <- stinziano_etal_2017 %>%
  
  # The dataset contains multiple RACiR curves, so we will filter out all but one
  filter(curve_type == "RACiR 0 to 500", plant_id == "A") %>%
  
  # Set units
  mutate(
    Ci    = set_units( Ci,    umol / mol     ),
    A     = set_units( A,     umol / m^2 / s ),
    Tleaf = set_units( Tleaf, degreeC        ),
    PARi  = set_units( PARi,  umol / m^2 / s ),
    E     = set_units( E,     mol / m^2 / s  ),
    gsc   = set_units( gsc,   mol / m^2 / s  ),
    Cs    = set_units( Cs,    umol / mol     ),
    Cr    = set_units( Cr,    umol / mol     ),
    Pa    = set_units( Pa,    kPa            )
  ) %>%
  
  # Following Sharkey et al. (2007), bayCi uses Pa units
  mutate(
    Pca = set_units(Cr * Pa, Pa),
    Pci = set_units(Ci * Pa, Pa),
    Pgsc = convert_conductance(gsc, P = Pa, Temp = Tleaf)$`umol/m^2/s/Pa`
  )

rcr <- racir(df)

df <- stinziano_etal_2017_empty %>%
  
  # Set units
  mutate(
    A     = set_units( A,     umol / m^2 / s ),
    Pa    = set_units( Pa,    kPa            ),
    Pca   = set_units( Pca,   Pa             ),
    Tleaf = set_units( Tleaf, degreeC        )
  )

mty <- empty(df)

fit <- braycir(rcr, mty)