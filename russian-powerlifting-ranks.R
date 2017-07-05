library(rvest)
library(tidyverse)
library(plotly)


file_name <- "russian-powerlifting-ranks.rds"

if (file.exists(file_name)) {
	norms <- readRDS(file_name)
	ranks <- names(norms)[-1]
} else {
	ranks <- c(
		"Elite",
		"Master of Sport, International Class",
		"Master of Sport",
		"Candidate for Master of Sport",
		"First-Class Sportsman",
		"Second-Class Sportsman",
		"Third-Class Sportsman",
		"First-Class Junior Sportsman",
		"Second-Class Junior Sportsman"
	)

	norms <- read_html("http://www.wpc-wpo.ru/rules/Razryadnyie_normativyi_AWPC_pauerlifting_-_bez_ekipirovki.html") %>% 
		html_nodes("table") %>% 
		.[[2]] %>% 
		html_table(header = TRUE) %>% 
		select(-1) %>% 
		set_names(c("weight", ranks)) %>% 
		mutate(weight = ifelse(weight == "140+", Inf, as.numeric(weight)))

	saveRDS(norms, file_name)
}


df <- expand.grid(
	seq(52, 155, .5),
	seq(150, 1000, 2.5),
	NA
) %>% set_names(c("weight", "total", "rank"))


for (rank in rev(ranks))
	for (i in 1:nrow(norms)) df[df$weight <= norms$weight[i] & df$total >= norms[[rank]][i], ]$rank <- rank


plot_ly(
	color = ~rank,
	colors = "Set1",
	x = ~weight,
	y = ~total
) %>% 
add_markers(
	data = df %>% filter(complete.cases(.)),
	hoverinfo = "text",
	hoverlabel = list(
		bgcolor = "white",
		color = "black"
	),
	opacity = 0,
	text = ~sprintf("%s @ %s = %s", total, weight, rank)
) %>% 
add_lines(
	data = norms %>% mutate(weight = ifelse(weight < Inf, weight, 155)) %>% gather(rank, total, -weight),
	hoverinfo = "skip",
	line = list(
		color = "black",
		shape = "vh",
		width = .5
	),
	opacity = .5
) %>% 
add_annotations(
	data = norms %>% select(-weight) %>% tail(1) %>% gather(rank, total) %>% mutate(weight = 155),
	showarrow = FALSE,
	text = ~rank,
	xanchor = "right",
	yanchor = "bottom"
) %>% 
layout(
	showlegend = FALSE,
	title = "Russian Powerlifting Ranks (<a href='http://www.wpc-wpo.ru/rules/Razryadnyie_normativyi_AWPC_pauerlifting_-_bez_ekipirovki.html'>wpc-wpo.ru</a>)",
	xaxis = list(
		dtick = 10,
		range = c(50, 155),
		showgrid = FALSE,
		showspikes = TRUE,
		spikecolor = "black",
		spikethickness = .5,
		title = "Bodyweight (kg)"
	),
	yaxis = list(
		dtick = 100,
		range = c(150, 1000),
		showgrid = FALSE,
		showspikes = TRUE,
		spikecolor = "black",
		spikethickness = .5,
		title = "Bench + squat + deadlift (kg)"
	)
)
