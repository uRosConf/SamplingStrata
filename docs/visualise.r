visualise <- function(output){
output<-as.data.frame(outout)
output$id <-as.character(rownames(output))
output$id <- factor (output$id, levels = output$id[length(output$id):1])
output.m<-melt(output )
output.m$grp<-as.character(rownames(output))
#output$id <- paste
ggplot(output.m, aes(value,id,fill=value))+ geom_tile(aes(fill = value))+ scale_fill_gradient(low="yellow", high="red")+facet_wrap(~variable)
}
