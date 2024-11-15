library(haven)
warzone = read_dta("GEIH_2023_mn_all.dta")

sub_warzone = warzone[, 
            c("directorio","secuencia_p","orden","hogar","dpto","p3271","p6040","p3042","oci","dsi","inglabo")]

sub_warzone$oci[is.na(sub_warzone$oci)] = 0
sub_warzone$dsi[is.na(sub_warzone$dsi)] = 0
sub_warzone$inglabo[is.na(sub_warzone$inglabo)] = 0

colnames(sub_warzone)[colnames(sub_warzone) == "p3042"] = "mayor_educacion"

write.csv(sub_warzone,"warzone.csv",row.names = FALSE)