getDatasetList <- function() {

	data_list <- data.frame(list.files(pattern="*.RData"))
	names(data_list)<-"data_sets"
	data_list <- do.call(rbind, strsplit(as.character(data_list$data_sets), " "))[,1]
	#get(data_list[1])
	return(data_list)

}
