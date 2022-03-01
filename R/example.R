# Example -----------------------------------------------------------------
CA <- spnaf::CA

OD <- cbind(CA$FIPS.County.Code.of.Geography.B, CA$FIPS.County.Code.of.Geography.A)
OD <- cbind(OD, CA$Flow.from.Geography.B.to.Geography.A)
OD <- data.frame(OD)
names(OD) <- c("oid", "did", "n")
OD$n <- as.numeric(OD$n)
OD <- OD[order(OD[,1], OD[,2]),]

head(OD)

CA_polygon <- spnaf::CA_polygon
head(OD)

# Test -------------------------------------------------------------------------
result <- spnaf(df = OD, shape = CA_polygon,
                queen = TRUE, snap = 1, # spdep options 
                method = 't', n = 10000)

head(result[[2]])