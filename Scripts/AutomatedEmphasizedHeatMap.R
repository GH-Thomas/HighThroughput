### IMPORTANT ### this function assumes that the group size is not the first column. If it is the rowname, it will be fine
### IMPORTANT ### requires tidyr and ggplot2

library(tidyverse)

#argument definitons:
	#gname: name of graph, subgname: subtitle, xlab: label for x axis, ylab = label for y axis, 
	#scalelab: scale label, graphlowcol: lower color of graph, graphhighcol: higher color of graph, decimals: decimals to round to on the graph,
	#slcutoff: small/large cutoff for number modifiers, smallsize: small number size, largesize: large number size, smallcolor: small number color, largecolor: large number color
ehm <- function(data, gname = "", subgname = '', xlab = 'Effect Size', ylab = '# of Mice per Group',
				 scalelab = 'Percentage', graphlowcol = 'white', graphhighcol = 'green', decimals = 0, slcutoff = 90, smallsize = 4, largesize = 4, smallcolor= 1, largecolor = 1){
	dataehm <- data.frame(data)
	dataehm <- gather(dataehm, 'dose')
	dataehm$ngroups <- rep(sort(as.numeric(as.character(rownames(data)))), ncol(data))
	dataehm$rvalue <- round(dataehm$value, decimals)
	#possibly code if-else to make argument calling easier - have a size that is used if smallsize and largesize are the same, same for color
	dataehm$ngroups <- as.factor(dataehm$ngroups)
	levels(dataehm$ngroups) <- rownames(data)
	dataehm$pgood <- smallsize
	dataehm$pgood[dataehm$rvalue >= slcutoff] <- largesize
	dataehm$pgood2 <- smallcolor
	dataehm$pgood2[dataehm$rvalue >= slcutoff] <- largecolor
	ggplot(dataehm, aes(main = gname, x = dose, y = ngroups, label = rvalue)) + geom_tile(aes(fill = dataehm$rvalue)) + 
	scale_fill_gradient(low = graphlowcol, high = graphhighcol) + geom_text(size = dataehm$pgood, color = dataehm$pgood2) + labs(title = gname, subtitle = subgname, x = xlab, y = ylab, fill = scalelab)
}

#example i was using
	#ehm(as.data.frame(test2), c(2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 60), 10, 'testgraph')

