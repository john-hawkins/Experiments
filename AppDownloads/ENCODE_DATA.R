library(tidyr)
library(data.table)
library(dplyr)

source("ENCODE_DATA_FUNCTION.R")

# ##############################################################################
# LOAD THE TRAINING DATA, BREAK IT INTO PARTS AND ENCODE
# NOTE: THE ENCODING PROCESS BREAKS IF YOU TRY AND DO ALL DATA AT ONCE
# ##############################################################################
encodeData 		<- readRDS("data/train_data_to_encode_round2.rds")

groups			<- 12

encodeData$randGroup	<- sample(groups, size = nrow(encodeData), replace = TRUE)

# TARGET IS CATEGORICAL
encodeData$is_attributed = as.factor(encodeData$is_attributed)

for(g in seq(1,groups) ) {
	dset    	<- encodeData[encodeData$randGroup==g,]
	encData  	<- encodeDataset(dset)
	spath		<- paste("data/train_set_part", g, ".rds", sep="")
	saveRDS(encData, spath)
	rm(encData)
	rm(dset)
}


